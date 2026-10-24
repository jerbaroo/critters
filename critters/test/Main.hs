module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Critters.Engine qualified as Engine
import Critters.Event qualified as Event
import Critters.Input qualified as Input
import Critters.Interface.TUI qualified as TUI
import Critters.Save qualified as Save
import Critters.State (GameState (..), Position (..), Tile (..), World (..), initialState, tileAt)
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.IO qualified as TextIO
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

assertRoundTrip :: (Eq a, Aeson.FromJSON a, Aeson.ToJSON a) => a -> Assertion
assertRoundTrip value =
  assertBool "JSON should preserve the value" $
    Aeson.eitherDecode (Aeson.encode value) == Right value

engineTests :: TestTree
engineTests =
  testGroup
    "Engine"
    [ testCase "Apply recorded facts without checking movement rules"
        $ assertBool "Expected an absolute recorded position, preserving the world"
        $ Engine.applyEvent state (Event.PlayerMoved (Position (-4) 9))
          == state{playerPosition = Position (-4) 9},
      testCase "Directions use the bottom-left coordinate system"
        $ forM_
          [ (Input.Down, Position 2 1),
            (Input.Left, Position 1 2),
            (Input.Right, Position 3 2),
            (Input.Up, Position 2 3)
          ]
        $ \(input, destination) ->
          assertBool "Expected the input to produce and apply a movement fact" $
            Engine.step state input == (state{playerPosition = destination}, [Event.PlayerMoved destination]),
      testCase "Replay history reproduces sequential input processing" $
        let (finalState, history) = foldl advance (state, []) [Input.Up, Input.Right, Input.Down]
         in assertBool "Expected each input to use the latest state, with replayable facts" $
              finalState == state{playerPosition = Position 3 2}
                && history == map Event.PlayerMoved [Position 2 3, Position 3 3, Position 3 2]
                && foldl Engine.applyEvent state history == finalState,
      testCase "Unmapped and non-movement inputs leave state unchanged" $
        forM_ [Input.Character 'x', Input.Character 'q', Input.Enter, Input.Escape] $
          \input -> assertBool "Expected no state changes or events" (Engine.step state input == (state, []))
    ]
  where
    advance (current, history) input =
      let (next, events) = Engine.step current input
       in (next, history <> events)
    state = GameState (Position 2 2) (World (replicate 5 (replicate 5 Ground)))

eventTests :: TestTree
eventTests =
  testGroup
    "Event"
    [ testCase "Load an independently written fixture"
        $ assertBool "Expected the recorded destination"
        $ Aeson.eitherDecode fixture == Right event,
      testCase "Reject a missing position"
        $ assertBool "Expected a parse error"
        $ isLeft (Aeson.eitherDecode @Event.Event "{\"type\":\"player-moved\"}"),
      testCase "Reject an unknown event type"
        $ assertBool "Expected a parse error"
        $ isLeft (Aeson.eitherDecode @Event.Event "{\"position\":{\"x\":2,\"y\":1},\"type\":\"walk\"}"),
      testCase "Round trips" $
        forM_ [Position (-1) 2, Position minBound maxBound] (assertRoundTrip . Event.PlayerMoved),
      testCase "Save the expected JSON structure"
        $ assertBool "Expected an event type and an absolute position"
        $ Aeson.eitherDecode fixture == Right (Aeson.toJSON event)
    ]
  where
    event = Event.PlayerMoved (Position 2 1)
    fixture = "{\"position\":{\"x\":2,\"y\":1},\"type\":\"player-moved\"}"

inputTests :: TestTree
inputTests =
  testGroup
    "Input"
    [ testCase "Reject multiple characters after trimming"
        $ assertBool "Expected unknown input"
        $ Input.parse "  xy  " == Nothing,
      testCase "Strip surrounding whitespace" $
        forM_ [(" \tdown\r\n", Input.Down), (" \t🐱\n ", Input.Character '🐱')] $
          \(raw, expected) -> assertBool "Expected the trimmed control" (Input.parse raw == Just expected),
      testCase "Whitespace-only input means Enter"
        $ assertBool "Expected Enter"
        $ Input.parse " \t\r\n " == Just Input.Enter
    ]

main :: IO ()
main = defaultMain tests

saveTests :: TestTree
saveTests =
  testGroup
    "Save"
    [ testCase "Apply history in order without rechecking rules"
        $ assertBool "Expected the final recorded position and original world"
        $ Save.apply initialState history == initialState{playerPosition = Position (-4) 9},
      testCase "Empty history preserves the starting state"
        $ assertBool "Expected the original state"
        $ Save.apply initialState [] == initialState,
      testCase "Load an independently written history" $
        withSaveFile $ \path ->
          forM_ ["\n", "\r\n"] $ \newline ->
            forM_ ["", newline] $ \ending -> do
              TextIO.writeFile path (Text.intercalate newline eventLines <> ending)
              result <- Save.load path
              assertBool "Expected ordered events with either newline convention and an optional final newline" (result == Right history),
      testCase "Load legacy JSON-array histories" $
        withSaveFile $ \path -> do
          TextIO.writeFile path ("[" <> Text.intercalate "," eventLines <> "]")
          result <- Save.load path
          assertBool "Expected existing histories to remain readable" (result == Right history),
      testCase "Read failures are returned" $
        withSaveFile $ \path -> do
          result <- Save.load (path <> "/history.json")
          assertBool "Expected a file error" (isLeft result),
      testCase "Reject invalid histories" $
        withSaveFile $ \path ->
          forM_ ["{", "{}", "{\"type\":\"unknown\"}\n", "{\"position\":{\"x\":2,\"y\":1},\"type\":\"player-moved\"}\n{}\n", "\n", "[{\"type\":\"unknown\"}]"] $ \invalid -> do
            TextIO.writeFile path invalid
            result <- Save.load path
            assertBool "Expected the entire history to be rejected" (isLeft result),
      testCase "Round trip engine histories through disk" $
        withSaveFile $ \path -> do
          let (nextState, events) = Engine.step initialState Input.Right
          forM_ [(events, nextState), ([], initialState)] $ \(savedEvents, expectedState) -> do
            written <- Save.save path savedEvents
            loaded <- Save.load path
            assertBool "Expected writing to succeed" (written == Right ())
            assertBool "Expected the complete saved history" (loaded == Right savedEvents)
            assertBool "Expected replay to reproduce the resulting state" (fmap (Save.apply initialState) loaded == Right expectedState),
      testCase "Write independently decodable events on separate lines" $
        withSaveFile $ \path ->
          forM_ [history, []] $ \savedEvents -> do
            written <- Save.save path savedEvents
            contents <- TextIO.readFile path
            let decoded = traverse (Aeson.eitherDecodeStrict @Event.Event . TextEncoding.encodeUtf8) (Text.lines contents)
            assertBool "Expected writing to succeed" (written == Right ())
            assertBool "Expected one event per line in order" (decoded == Right savedEvents)
            assertBool "Expected an empty file or a final newline" $
              if null savedEvents then Text.null contents else Text.isSuffixOf "\n" contents,
      testCase "Write failures are returned" $
        withSaveFile $ \path -> do
          result <- Save.save (path <> "/history.json") history
          assertBool "Expected a file error" (isLeft result)
    ]
  where
    eventLines =
      [ "{\"position\":{\"x\":2,\"y\":1},\"type\":\"player-moved\"}",
        "{\"position\":{\"x\":-4,\"y\":9},\"type\":\"player-moved\"}"
      ]
    history = map Event.PlayerMoved [Position 2 1, Position (-4) 9]

stateTests :: TestTree
stateTests =
  testGroup
    "State"
    [ testGroup
        "Decode errors"
        [ testCase "Fractional coordinate"
            $ assertBool "Expected a parse error"
            $ isLeft (Aeson.eitherDecode @Position "{\"x\":1.5,\"y\":0}"),
          testCase "Invalid JSON"
            $ assertBool "Expected a parse error"
            $ isLeft (Aeson.eitherDecode @GameState "{"),
          testCase "Missing coordinate"
            $ assertBool "Expected a parse error"
            $ isLeft (Aeson.eitherDecode @Position "{\"x\":1}"),
          testCase "Missing save version"
            $ assertBool "Expected a parse error"
            $ isLeft
              ( Aeson.eitherDecode @GameState
                  "{\"playerPosition\":{\"x\":0,\"y\":0},\"world\":{\"tiles\":[]}}"
              ),
          testCase "Non-string tile"
            $ assertBool "Expected a parse error"
            $ isLeft (Aeson.eitherDecode @Tile "1"),
          testCase "Out-of-range coordinate"
            $ assertBool "Expected a parse error"
            $ isLeft (Aeson.eitherDecode @Position "{\"x\":1e100,\"y\":0}"),
          testCase "Unknown tile"
            $ assertBool "Expected a parse error"
            $ isLeft (Aeson.eitherDecode @Tile "\"lava\""),
          testCase "Unknown world tile"
            $ assertBool "Expected a parse error"
            $ isLeft (Aeson.eitherDecode @World "{\"tiles\":[[\"lava\"]]}"),
          testCase "Unsupported save version"
            $ assertBool "Expected a parse error"
            $ isLeft
              ( Aeson.eitherDecode @GameState
                  "{\"playerPosition\":{\"x\":0,\"y\":0},\"version\":2,\"world\":{\"tiles\":[]}}"
              )
        ],
      testGroup
        "JSON structure"
        [ testCase "Load an independently written fixture"
            $ assertBool "Expected the fixture's state"
            $ Aeson.eitherDecode fixture == Right state,
          testCase "Save the expected JSON structure"
            $ assertBool "Expected the fixture's JSON fields"
            $ Aeson.eitherDecode fixture == Right (Aeson.toJSON state)
        ],
      testGroup
        "Round trips"
        [ testCase "GameState" $ assertRoundTrip initialState,
          testCase "Position" $
            forM_ [Position 0 0, Position (-1) 2, Position minBound maxBound] assertRoundTrip,
          testCase "Tile" $
            forM_ [Ground, TallGrass, Wall, Water] assertRoundTrip,
          testCase "World" $
            forM_ [World [], World [[]], World [[Ground], [], [Water, Wall]]] assertRoundTrip
        ],
      testGroup
        "World coordinates"
        [ testCase "Bottom-left origin with y increasing upwards"
            $ assertBool "Expected columns to run rightwards and rows upwards"
            $ map (tileAt sampleWorld) [Position 0 0, Position 0 1, Position 1 0, Position 1 1]
              == map Just [Ground, TallGrass, Water, Wall],
          testCase "Missing cells"
            $ assertBool "Expected empty worlds and missing cells to return Nothing"
            $ tileAt (World []) (Position 0 0) == Nothing
              && tileAt (World [[]]) (Position 0 0) == Nothing
              && tileAt (World [[Ground], []]) (Position 0 0) == Nothing
              && tileAt (World [[Ground], []]) (Position 0 1) == Just Ground,
          testCase "Outside the world"
            $ forM_
              [ Position (-1) 0,
                Position 0 (-1),
                Position 0 2,
                Position 2 0,
                Position maxBound maxBound,
                Position minBound minBound
              ]
            $ \position -> assertBool "Expected Nothing outside the world" (tileAt sampleWorld position == Nothing)
        ]
    ]
  where
    fixture =
      "{\"playerPosition\":{\"x\":2,\"y\":1},\"version\":1,\"world\":{\"tiles\":[[\"ground\",\"wall\"],[\"water\",\"tall-grass\"]]}}"
    sampleWorld = World [[TallGrass, Wall], [Ground, Water]]
    state = GameState (Position 2 1) (World [[Ground, Wall], [Water, TallGrass]])

tests :: TestTree
tests = testGroup "Critters" [engineTests, eventTests, inputTests, saveTests, stateTests, tuiTests]

tuiTests :: TestTree
tuiTests =
  testGroup
    "TUI"
    [ testCase "Camera follows the player and stops at world edges"
        $ forM_
          [ (Position 0 0, (0, 0)),
            (Position 0 (TUI.height * 2 - 1), (0, TUI.height - 1)),
            (Position TUI.width TUI.height, (TUI.width `div` 2, TUI.height `div` 2)),
            (Position (TUI.width * 2 - 1) 0, (TUI.width - 1, 0)),
            (Position (TUI.width * 2 - 1) (TUI.height * 2 - 1), (TUI.width - 1, TUI.height - 1))
          ]
        $ \(position, destination) ->
          assertBool "Expected the player at the centered or clamped viewport position" $
            tile (view (GameState position largeWorld)) destination == "@ ",
      testCase "Empty and ragged worlds retain a fixed viewport" $
        forM_ [World [], World [[]], World [[Wall], [], [Ground, Water]]] $ \layout -> do
          let rows = view (GameState (Position minBound maxBound) layout)
          assertBool "Expected the configured rows and two-column tiles without borders" $
            length rows == TUI.height && all ((== TUI.width * 2) . Text.length) rows
          assertBool "Expected out-of-viewport players to be omitted" $
            all (not . Text.isInfixOf "@") rows,
      testCase "Movement appears above the old position when walking north" $
        let state = GameState (Position 2 2) largeWorld
            (nextState, _) = Engine.step state Input.Up
         in assertBool "Expected the player one row higher, with the old terrain restored" $
              tile (view state) (2, 2) == "@ "
                && tile (view nextState) (2, 3) == "@ "
                && tile (view nextState) (2, 2) == ". ",
      testCase "Render tiles north-up with the player over the terrain" $
        let rows = view (GameState (Position 0 0) (World [[TallGrass, Wall, Ground], [Ground, Water, Wall]]))
         in assertBool "Expected all tile glyphs, padding, and the player at the bottom-left" $
              Text.take 6 (rows !! (TUI.height - 2)) == "^ ##. "
                && Text.take 6 (rows !! (TUI.height - 1)) == "@ ~~##"
                && tile rows (3, 0) == "  "
                && tile rows (0, TUI.height - 1) == "  "
    ]
  where
    largeWorld = World (replicate (TUI.height * 2) (replicate (TUI.width * 2) Ground))
    tile rows (column, row) = Text.take 2 $ Text.drop (column * 2) (rows !! (TUI.height - row - 1))
    view = Text.lines . TUI.render

withSaveFile :: (FilePath -> IO a) -> IO a
withSaveFile action = do
  directory <- getTemporaryDirectory
  bracket
    ( do
        (path, handle) <- openTempFile directory "critters-save.json"
        hClose handle
        pure path
    )
    removeFile
    action
