module Main (main) where

import BattleEventTests qualified as BattleEventTests
import BattlePersistenceTests qualified as BattlePersistenceTests
import BattleTests qualified as BattleTests
import Control.Exception (bracket)
import Control.Monad (forM_)
import Critters.Command qualified as Command
import Critters.Engine qualified as Engine
import Critters.Event qualified as Event
import Critters.Input qualified as Input
import Critters.Input.Direction (Direction (..))
import Critters.Interface.TUI qualified as TUI
import Critters.Interface.TUI.Cell (Cell (..))
import Critters.Interface.TUI.Cell qualified as Cell
import Critters.Save qualified as Save
import Critters.State (Condition (..), Conversation (..), Critter (..), FlagId (..), GameState (..), Location (..), MapId (..), Npc (..), NpcId (..), Party (..), Position (..), Tile (..), Warp (..), World (..), basicCritter, conversationText, initialState, tileAt, world)
import Critters.State.Critter qualified as Critter
import Critters.State.GameState qualified as GameState
import Critters.State.Npc qualified as Npc
import Critters.State.World qualified as World
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.IO qualified as TextIO
import InteractionTests qualified as InteractionTests
import PlaybackTests qualified as PlaybackTests
import SceneTests qualified as SceneTests
import SchemaTests qualified as SchemaTests
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import TurnEventTests qualified as TurnEventTests

assertRoundTrip :: (Eq a, Aeson.FromJSON a, Aeson.ToJSON a) => a -> Assertion
assertRoundTrip value =
    assertBool "JSON should preserve the value" $
        Aeson.eitherDecode (Aeson.encode value) == Right value

conditionTests :: TestTree
conditionTests =
    testGroup
        "Condition"
        [ testCase "Evaluate present and missing flags" $ do
            forM_ [Map.empty, Map.singleton (FlagId "other") True, Map.singleton identifier False] $ \storyFlags ->
                let state = initialState{GameState.flags = storyFlags}
                 in assertBool "Expected missing and explicitly false flags to match false, regardless of unrelated flags" $
                        Engine.conditionHolds state (Flag identifier False)
                            && not (Engine.conditionHolds state (Flag identifier True))
            let state = initialState{GameState.flags = Map.singleton identifier True}
            assertBool "Expected a true flag to match only true" $
                Engine.conditionHolds state (Flag identifier True)
                    && not (Engine.conditionHolds state (Flag identifier False))
        , testCase "Load and encode independently written flag conditions"
            $ forM_
                [ ("{\"type\":\"flag\",\"flag\":\"guide.offer-made\",\"value\":false}", Flag identifier False)
                , ("{\"type\":\"flag\",\"flag\":\"guide.offer-made\",\"value\":true}", Flag identifier True)
                ]
            $ \(encoded, expected) ->
                assertBool "Expected the flag discriminator, identifier and boolean value" $
                    Aeson.eitherDecode encoded == Right expected
                        && Aeson.eitherDecode encoded == Right (Aeson.toJSON expected)
        , testCase "Load story flags and defaults in current and legacy game files" $
            forM_ ["\"maps\":{\"overworld\":{\"tiles\":[]}}", "\"world\":{\"tiles\":[]}"] $ \layout ->
                forM_
                    [ ("", Map.empty)
                    , (",\"flags\":{\"guide.offer-made\":true,\"lab.critter-claimed\":false}", Map.fromList [(identifier, True), (FlagId "lab.critter-claimed", False)])
                    ]
                    $ \(fields, expected) ->
                        let encoded = "{" <> layout <> fields <> ",\"playerPosition\":{\"x\":0,\"y\":0},\"version\":1}"
                         in assertBool "Expected flag object keys and values to survive loading, with an empty default" $
                                fmap (.flags) (Aeson.eitherDecode @GameState encoded) == Right expected
        , testCase "Reject incomplete or invalid flag conditions"
            $ forM_
                [ "{\"flag\":\"guide.offer-made\",\"value\":true}"
                , "{\"type\":\"flag-is\",\"flag\":\"guide.offer-made\",\"value\":true}"
                , "{\"type\":\"flag\",\"value\":true}"
                , "{\"type\":\"flag\",\"flag\":\"guide.offer-made\"}"
                , "{\"type\":\"flag\",\"flag\":1,\"value\":true}"
                , "{\"type\":\"flag\",\"flag\":\"guide.offer-made\",\"value\":\"true\"}"
                , "{\"type\":\"flag\",\"flag\":\"guide.offer-made\",\"value\":null}"
                ]
            $ \invalid -> assertBool "Expected a flag identifier and explicit boolean value with type flag" (isLeft (Aeson.eitherDecode @Condition invalid))
        , testCase "Reject invalid story flags" $
            forM_ ["\"maps\":{\"overworld\":{\"tiles\":[]}}", "\"world\":{\"tiles\":[]}"] $ \layout ->
                forM_ ["null", "[]", "{\"guide.offer-made\":1}", "{\"guide.offer-made\":\"true\"}"] $ \invalid ->
                    let encoded = "{" <> layout <> ",\"flags\":" <> invalid <> ",\"playerPosition\":{\"x\":0,\"y\":0},\"version\":1}"
                     in assertBool "Expected flags to be an object of boolean values" (isLeft (Aeson.eitherDecode @GameState encoded))
        , testCase "Round trip game-defined identifiers, conditions and story flags" $
            forM_ [identifier, FlagId "門"] $ \name -> do
                assertRoundTrip name
                forM_ [False, True] $ \value -> do
                    assertRoundTrip (Flag name value)
                    assertRoundTrip initialState{GameState.flags = Map.singleton name value}
        ]
  where
    identifier = FlagId "guide.offer-made"

conversationTests :: TestTree
conversationTests =
    testGroup
        "Conversation"
        [ testCase "Advance and finish a conversation" $
            let (opened, started) = Engine.step npcState Input.Enter
                (advanced, progressed) = Engine.step opened Input.Enter
                (closed, ended) = Engine.step advanced Input.Enter
                (reopened, restarted) = Engine.step closed Input.Enter
             in assertBool "Expected two dialogue lines, closing after the last, and restarting at the first" $
                    started == [Event.ConversationStarted (NpcId "guide")]
                        && opened.conversation == Just (Conversation 0 (NpcId "guide"))
                        && conversationText opened == Just "Hello!"
                        && progressed == [Event.ConversationAdvanced 1]
                        && advanced.conversation == Just (Conversation 1 (NpcId "guide"))
                        && conversationText advanced == Just "Visit my lab."
                        && ended == [Event.ConversationEnded]
                        && closed == npcState
                        && reopened.conversation == opened.conversation
                        && restarted == started
        , testCase "Block NPC cells and turn towards them" $
            let current = npcState{GameState.playerDirection = North}
                (turned, events) = Engine.step current Input.Right
             in assertBool "Expected facing to change while the NPC prevents movement" $
                    turned == npcState
                        && events == [Event.PlayerTurned East]
                        && snd (Engine.step turned Input.Enter) == [Event.ConversationStarted (NpcId "guide")]
        , testCase "Block NPCs at ledge landings" $
            let current =
                    npcState
                        { GameState.playerDirection = South
                        , GameState.playerPosition = Position 2 3
                        , GameState.maps = Map.singleton (MapId "overworld") (filledWorld [replicate 4 Ground, [Ground, Ground, Ledge South, Ground], replicate 4 Ground, replicate 4 Ground])
                        }
             in assertBool "Expected no jump onto an NPC" $
                    Engine.step current Input.Down == (current, [])
        , testCase "Choose commands from conversation context" $
            let active = npcState{GameState.conversation = Just (Conversation 0 (NpcId "guide"))}
             in assertBool "Expected context-sensitive Enter and Escape, with quit always available" $
                    Engine.commandFor npcState Input.Enter == Just Command.Interact
                        && Engine.commandFor active Input.Enter == Just Command.AdvanceConversation
                        && Engine.commandFor active Input.Escape == Just Command.EndConversation
                        && Engine.commandFor npcState (Input.Character 'q') == Just Command.Quit
                        && Engine.commandFor active (Input.Character 'q') == Just Command.Quit
        , testCase "End a conversation with Escape" $
            let active = npcState{GameState.conversation = Just (Conversation 0 (NpcId "guide"))}
             in assertBool "Expected closing dialogue to restore exploration" $
                    Engine.step active Input.Escape == (npcState, [Event.ConversationEnded])
        , testCase "Ignore conversation commands outside a conversation" $
            forM_ [Command.AdvanceConversation, Command.EndConversation] $ \command ->
                assertBool "Expected no conversation facts without an active conversation" $
                    Engine.eventsFor npcState command == []
        , testCase "Ignore movement while talking" $
            forM_ [(East, Input.Right), (North, Input.Up), (South, Input.Down), (West, Input.Left), (West, Input.Character 'h'), (South, Input.Character 'j'), (North, Input.Character 'k'), (East, Input.Character 'l')] $ \(direction, input) ->
                let active = npcState{GameState.conversation = Just (Conversation 0 (NpcId "guide"))}
                 in assertBool "Expected movement to be ignored even through direct command execution" $
                        Engine.commandFor active input == Nothing
                            && Engine.step active input == (active, [])
                            && Engine.eventsFor active (Command.Walk direction) == []
                            && Engine.eventsFor active Command.Interact == []
        , testCase "Ignore NPCs outside the facing cell" $
            forM_ [North, South, West] $ \direction ->
                let current = npcState{GameState.playerDirection = direction}
                 in assertBool "Expected interaction to require the adjacent cell in front" $
                        Engine.step current Input.Enter == (current, [])
        , testCase "Ignore NPCs with no dialogue" $
            let current = npcState{GameState.npcs = Map.singleton (NpcId "guide") (Npc [] [] (Location (MapId "overworld") (Position 2 1)) "Guide")}
             in assertBool "Expected an empty dialogue to produce no conversation" $
                    Engine.step current Input.Enter == (current, [])
        , testCase "Read missing or invalid conversation lines safely" $
            forM_ [Conversation (-1) (NpcId "guide"), Conversation 2 (NpcId "guide"), Conversation 0 (NpcId "missing")] $ \active ->
                let current = npcState{GameState.conversation = Just active}
                 in assertBool "Expected no displayed text and a clean end when advancing" $
                        conversationText current == Nothing
                            && Engine.step current Input.Enter == (npcState, [Event.ConversationEnded])
        , testCase "Replay conversation facts without rechecking their preconditions" $
            let active = Save.apply npcState [Event.ConversationStarted (NpcId "missing"), Event.ConversationAdvanced 42, Event.PlayerTurned West]
             in assertBool "Expected facts to apply without looking up the NPC or validating dialogue" $
                    active == npcState{GameState.conversation = Just (Conversation 42 (NpcId "missing")), GameState.playerDirection = West}
                        && Save.apply active [Event.ConversationEnded] == npcState{GameState.playerDirection = West}
        , testCase "Walk again after closing dialogue" $
            let active = npcState{GameState.conversation = Just (Conversation 0 (NpcId "guide"))}
                (closed, _) = Engine.step active Input.Escape
             in assertBool "Expected normal turning and movement after closing" $
                    Engine.step closed Input.Up
                        == (npcState{GameState.playerDirection = North, GameState.playerPosition = Position 1 2}, [Event.PlayerTurned North, Event.PlayerMoved (Position 1 2)])
        ]

critterTests :: TestTree
critterTests =
    testGroup
        "Critter"
        [ testCase "Accept full, injured and fainted critters" $
            forM_ [0, 1, 20] $
                \current -> assertRoundTrip critter{Critter.hitPoints = current}
        , testCase "Load an independently written fixture" $
            assertBool "Expected the species, level and HP from the fixture" $
                Aeson.eitherDecode fixture == Right critter
        , testCase "Reject HP outside the valid range" $
            forM_ [-1, 21] $ \current ->
                assertBool "Expected current HP outside zero to maximum HP to fail decoding" $
                    isLeft (Aeson.eitherDecode @Critter (Aeson.encode critter{Critter.hitPoints = current}))
        , testCase "Reject missing or wrongly typed fields"
            $ forM_
                [ "{\"level\":5,\"maxHitPoints\":20,\"species\":\"leafling\"}"
                , "{\"hitPoints\":20,\"maxHitPoints\":20,\"species\":\"leafling\"}"
                , "{\"hitPoints\":20,\"level\":5,\"species\":\"leafling\"}"
                , "{\"hitPoints\":20,\"level\":5,\"maxHitPoints\":20}"
                , "{\"hitPoints\":1.5,\"level\":5,\"maxHitPoints\":20,\"species\":\"leafling\"}"
                , "{\"hitPoints\":20,\"level\":5,\"maxHitPoints\":20,\"species\":null}"
                ]
            $ \invalid -> assertBool "Expected a parse error" (isLeft (Aeson.eitherDecode @Critter invalid))
        , testCase "Reject nonpositive levels" $
            forM_ [-1, 0] $ \value ->
                assertBool "Expected a positive level to be required" $
                    isLeft (Aeson.eitherDecode @Critter (Aeson.encode critter{Critter.level = value}))
        , testCase "Reject nonpositive maximum HP" $
            forM_ [-1, 0] $ \value ->
                assertBool "Expected positive maximum HP even for a fainted critter" $
                    isLeft (Aeson.eitherDecode @Critter (Aeson.encode critter{Critter.hitPoints = 0, Critter.maxHitPoints = value}))
        , testCase "Reject blank species identifiers" $
            forM_ ["", " \t\n"] $ \identifier ->
                assertBool "Expected a nonblank species identifier to be required" $
                    isLeft (Aeson.eitherDecode @Critter (Aeson.encode critter{Critter.species = identifier}))
        , testCase "Round trip author-defined species and numeric boundaries" $
            forM_
                [ basicCritter 0 1 1 "leafling"
                , basicCritter maxBound maxBound maxBound "葉っぱ"
                ]
                assertRoundTrip
        , testCase "Save the expected JSON structure" $
            assertBool "Expected exactly the critter's four fields" $
                Aeson.eitherDecode fixture == Right (Aeson.toJSON critter)
        ]
  where
    critter = basicCritter 20 5 20 "leafling"
    fixture = "{\"hitPoints\":20,\"level\":5,\"maxHitPoints\":20,\"species\":\"leafling\"}"

engineTests :: TestTree
engineTests =
    testGroup
        "Engine"
        [ testCase "Apply recorded facts without checking movement rules" $
            assertBool "Expected an absolute recorded position, preserving the world" $
                Engine.applyEvent state (Event.PlayerMoved (Position (-4) 9))
                    == state{GameState.playerPosition = Position (-4) 9}
        , testCase "Block occupied ground and water without producing events" $
            forM_ [Fence, Arena, House, Laboratory, Ledge South, Shop, Clinic, Rock, Sign, Tree, Wall, Water] $ \tile ->
                let current = withDestination tile
                 in assertBool "Expected the original state and no movement fact" $
                        Engine.step current Input.Up == (current, [])
        , testCase "Cross ledges only in their direction" $
            forM_ [East, North, South, West] $ \allowed ->
                forM_
                    [ (East, Input.Right, Position 1 2, Position 3 2)
                    , (North, Input.Up, Position 2 1, Position 2 3)
                    , (South, Input.Down, Position 2 3, Position 2 1)
                    , (West, Input.Left, Position 3 2, Position 1 2)
                    ]
                    $ \(direction, input, origin, destination) ->
                        let current = (withLedge allowed){GameState.playerDirection = direction, GameState.playerPosition = origin}
                            expected =
                                if direction == allowed
                                    then (current{GameState.playerPosition = destination}, [Event.PlayerMoved destination])
                                    else (current, [])
                         in assertBool "Expected a single fact landing beyond the ledge only in its direction" $
                                Engine.step current input == expected
        , testCase "Directions use the bottom-left coordinate system"
            $ forM_
                [ (Input.Down, South, Position 2 1)
                , (Input.Left, West, Position 1 2)
                , (Input.Right, East, Position 3 2)
                , (Input.Up, North, Position 2 3)
                ]
            $ \(input, direction, destination) ->
                assertBool "Expected the input to produce and apply a movement fact" $
                    Engine.step state input == (state{GameState.playerDirection = direction, GameState.playerPosition = destination}, [Event.PlayerTurned direction | direction /= North] <> [Event.PlayerMoved destination])
        , testCase "Reject destinations outside the world or in missing cells" $ do
            forM_ [filledWorld [], filledWorld [[Ground]]] $ \layout ->
                forM_ [(Input.Down, South), (Input.Left, West), (Input.Right, East), (Input.Up, North)] $ \(input, direction) ->
                    let current = (gameState (Position 0 0) layout){GameState.playerDirection = direction}
                     in assertBool "Expected no movement beyond the world" $
                            Engine.step current input == (current, [])
            let ragged = gameState (Position 1 0) (filledWorld [[], [Ground, Ground]])
            assertBool "Expected a missing cell in a ragged row to block movement" $
                Engine.step ragged Input.Up == (ragged, [])
            let hole = gameState (Position 1 0) (World [[Just Ground, Nothing, Just Ground], replicate 3 (Just Ground)] [])
            assertBool "Expected a missing cell within a row to block movement" $
                Engine.step hole Input.Up == (hole, [])
        , testCase "Reject ledges with blocked or missing landings" $ do
            forM_ (Nothing : map Just [Fence, Arena, House, Laboratory, Ledge South, Shop, Clinic, Rock, Sign, Tree, Wall, Water]) $ \landing ->
                let layout =
                        World
                            [ replicate 3 (Just Ground)
                            , [Just Ground, Just (Ledge South), Just Ground]
                            , [Just Ground, landing, Just Ground]
                            ]
                            []
                    current = (gameState (Position 1 2) layout){GameState.playerDirection = South}
                 in assertBool "Expected no jump into a blocked tile or missing cell" $
                        Engine.step current Input.Down == (current, [])
            let edge = (gameState (Position 0 1) (filledWorld [[Ground], [Ledge South]])){GameState.playerDirection = South}
            assertBool "Expected no jump beyond the world" $
                Engine.step edge Input.Down == (edge, [])
        , testCase "Replay history reproduces sequential input processing" $
            let (finalState, history) = foldl advance (state, []) [Input.Up, Input.Right, Input.Down]
             in assertBool "Expected each input to use the latest state, with replayable facts" $
                    finalState == state{GameState.playerDirection = South, GameState.playerPosition = Position 3 2}
                        && history == [Event.PlayerMoved (Position 2 3), Event.PlayerTurned East, Event.PlayerMoved (Position 3 3), Event.PlayerTurned South, Event.PlayerMoved (Position 3 2)]
                        && foldl Engine.applyEvent state history == finalState
        , testCase "Replay ledge crossings as absolute movement facts" $
            let current = withLedge South
                (nextState, events) = Engine.step current Input.Down
             in assertBool "Expected a two-cell crossing to replay through one recorded destination" $
                    nextState == current{GameState.playerPosition = Position 2 1}
                        && events == [Event.PlayerMoved (Position 2 1)]
                        && Save.apply current events == nextState
        , testCase "Unmapped and non-movement inputs leave state unchanged" $
            forM_ [Input.Character 'x', Input.Character 'q', Input.Enter, Input.Escape] $
                \input -> assertBool "Expected no state changes or events" (Engine.step state input == (state, []))
        , testCase "Use hjkl as exploration aliases for the arrow keys" $
            forM_ [('h', Input.Left, West), ('j', Input.Down, South), ('k', Input.Up, North), ('l', Input.Right, East)] $ \(key, arrow, direction) ->
                forM_ [state, withDestination Water, withLedge South, warpState, npcState] $ \current ->
                    let input = Input.Character key
                     in assertBool "Expected the same walk command, resulting state and events as the corresponding arrow" $
                            Engine.commandFor current input == Just (Command.Walk direction)
                                && Engine.step current input == Engine.step current arrow
        , testCase "Walk across terrain and doorways" $
            forM_ [Door, Flowers, Grass, Ground, Path, Sand, Stairs, TallGrass] $ \tile ->
                let current = withDestination tile
                    destination = Position 1 2
                 in assertBool "Expected a movement fact and the new player position" $
                        Engine.step current Input.Up == (current{GameState.playerPosition = destination}, [Event.PlayerMoved destination])
        ]
  where
    advance (current, history) input =
        let (next, events) = Engine.step current input
         in (next, history <> events)
    state = gameState (Position 2 2) (filledWorld (replicate 5 (replicate 5 Ground)))
    withDestination tile = gameState (Position 1 1) (filledWorld [[Ground, tile, Ground], replicate 3 Ground, replicate 3 Ground])
    withLedge direction =
        let layout =
                filledWorld
                    [ replicate 5 Ground
                    , replicate 5 Ground
                    , [Ground, Ground, Ledge direction, Ground, Ground]
                    , replicate 5 Ground
                    , replicate 5 Ground
                    ]
         in (gameState (Position 2 3) layout){GameState.playerDirection = direction}

eventTests :: TestTree
eventTests =
    testGroup
        "Event"
        [ testCase "Load independently specified facts"
            $ forM_
                [ ("{\"lineIndex\":1,\"type\":\"conversation-advanced\"}", Event.ConversationAdvanced 1)
                , ("{\"type\":\"conversation-ended\"}", Event.ConversationEnded)
                , ("{\"npcId\":\"guide\",\"type\":\"conversation-started\"}", Event.ConversationStarted (NpcId "guide"))
                , ("{\"type\":\"party-closed\"}", Event.PartyClosed)
                , ("{\"type\":\"party-opened\"}", Event.PartyOpened)
                , ("{\"direction\":\"west\",\"type\":\"player-turned\"}", Event.PlayerTurned West)
                , ("{\"location\":{\"mapId\":\"room\",\"position\":{\"x\":0,\"y\":1}},\"type\":\"player-warped\"}", Event.PlayerWarped (Location (MapId "room") (Position 0 1)))
                ]
            $ \(encoded, expected) ->
                assertBool "Expected independently specified facts and their JSON fields" $
                    Aeson.eitherDecode encoded == Right expected
                        && Aeson.eitherDecode encoded == Right (Aeson.toJSON expected)
        , testCase "Load an independently written fixture" $
            assertBool "Expected the recorded destination" $
                Aeson.eitherDecode fixture == Right event
        , testCase "Reject a missing position" $
            assertBool "Expected a parse error" $
                isLeft (Aeson.eitherDecode @Event.Event "{\"type\":\"player-moved\"}")
        , testCase "Reject missing or invalid conversation and turning fields"
            $ forM_
                [ "{\"type\":\"conversation-advanced\"}"
                , "{\"type\":\"conversation-started\"}"
                , "{\"type\":\"player-turned\"}"
                , "{\"direction\":\"up\",\"type\":\"player-turned\"}"
                ]
            $ \invalid -> assertBool "Expected a parse error" (isLeft (Aeson.eitherDecode @Event.Event invalid))
        , testCase "Reject an unknown event type" $
            assertBool "Expected a parse error" $
                isLeft (Aeson.eitherDecode @Event.Event "{\"position\":{\"x\":2,\"y\":1},\"type\":\"walk\"}")
        , testCase "Round trips" $ do
            forM_ [Event.PartyClosed, Event.PartyOpened] assertRoundTrip
            forM_ [Position (-1) 2, Position minBound maxBound] (assertRoundTrip . Event.PlayerMoved)
            forM_ [Event.ConversationAdvanced 1, Event.ConversationEnded, Event.ConversationStarted (NpcId "guide"), Event.PlayerTurned East, Event.PlayerTurned North, Event.PlayerTurned South, Event.PlayerTurned West, Event.PlayerWarped (Location (MapId "room") (Position 0 1))] assertRoundTrip
        , testCase "Save the expected JSON structure" $
            assertBool "Expected an event type and an absolute position" $
                Aeson.eitherDecode fixture == Right (Aeson.toJSON event)
        ]
  where
    event = Event.PlayerMoved (Position 2 1)
    fixture = "{\"position\":{\"x\":2,\"y\":1},\"type\":\"player-moved\"}"

filledWorld :: [[Tile]] -> World
filledWorld rows = World (map (map Just) rows) []

gameState :: Position -> World -> GameState
gameState location layout = initialState{GameState.maps = Map.singleton (MapId "overworld") layout, GameState.playerPosition = location}

inputTests :: TestTree
inputTests =
    testGroup
        "Input"
        [ testCase "Reject multiple characters after trimming" $
            assertBool "Expected unknown input" $
                Input.parse "  xy  " == Nothing
        , testCase "Strip surrounding whitespace" $
            forM_ [(" \tdown\r\n", Input.Down), (" \t🐱\n ", Input.Character '🐱')] $
                \(raw, expected) -> assertBool "Expected the trimmed control" (Input.parse raw == Just expected)
        , testCase "Whitespace-only input means Enter" $
            assertBool "Expected Enter" $
                Input.parse " \t\r\n " == Just Input.Enter
        ]

main :: IO ()
main = defaultMain tests

npcState :: GameState
npcState =
    (gameState (Position 1 1) (filledWorld (replicate 4 (replicate 4 Ground))))
        { GameState.npcs = Map.singleton (NpcId "guide") (Npc ["Hello!", "Visit my lab."] [] (Location (MapId "overworld") (Position 2 1)) "Guide")
        , GameState.playerDirection = East
        }

partyRenderTests :: TestTree
partyRenderTests =
    testGroup
        "Party rendering"
        [ testCase "Clip a large party using the full viewport without a hint footer" $ do
            let critters = replicate TUI.height (basicCritter 1 1 1 "visible") <> [basicCritter 1 1 1 "hidden"]
                state = initialState{GameState.maxPartySize = length critters, GameState.party = Party critters, GameState.partyOpen = True}
                drawing = rows state
            assertViewport state
            assertBool "Expected party members to fill the viewport and overflow to be clipped" $
                Text.isPrefixOf "1. Visible" (drawing !! 2)
                    && not (Text.isInfixOf "Hidden" (Text.concat drawing))
                    && Text.count ". Visible" (Text.concat drawing) == TUI.height - 2
        , testCase "Display critter details in party order" $ do
            let state =
                    initialState
                        { GameState.maxPartySize = 9
                        , GameState.party = Party [basicCritter 7 5 20 "leafling", basicCritter 0 2 12 "brookling"]
                        , GameState.partyOpen = True
                        }
            assertViewport state
            assertBool "Expected the count, configured maximum, species, level and current/maximum HP" $
                map Text.stripEnd (take 4 (rows state))
                    == ["Party — 2 / 9", "", "1. Leafling  Lv 5  HP 7/20", "2. Brookling  Lv 2  HP 0/12"]
        , testCase "Display an empty party" $ do
            let state = initialState{GameState.partyOpen = True}
                drawing = rows state
            assertViewport state
            assertBool "Expected a zero count and empty-party message without a hint footer" $
                map Text.stripEnd (take 3 drawing) == ["Party — 0 / 6", "", "Your party is empty"]
                    && all (Text.null . Text.stripEnd) (drop 3 drawing)
        , testCase "Open visibly with p and restore the overworld with p or Escape" $
            forM_ [Input.Character 'p', Input.Escape] $ \closeInput ->
                let (opened, _) = Engine.step npcState (Input.Character 'p')
                    (closed, _) = Engine.step opened closeInput
                 in assertBool "Expected the party screen on opening and the original overworld frame on closing" $
                        case rows opened of
                            heading : _ ->
                                Text.isPrefixOf "Party — " heading
                                    && TUI.renderFrame opened /= TUI.renderFrame npcState
                                    && TUI.renderFrame closed == TUI.renderFrame npcState
                            [] -> False
        , testCase "Wrap long Unicode species names within the viewport" $ do
            let name = Text.replicate columns "界" <> "e\x0301\ESC\r"
                state = initialState{GameState.party = Party [basicCritter 7 5 20 name], GameState.partyOpen = True}
                drawing = Text.concat (rows state)
            assertViewport state
            assertBool "Expected wide characters and combining marks to survive wrapping, with terminal controls removed" $
                Text.isInfixOf "界" drawing
                    && Text.isInfixOf "e\x0301" drawing
                    && not (Text.any (\value -> value == '\ESC' || value == '\r') drawing)
        ]
  where
    assertViewport state =
        let frame = TUI.renderFrame state
         in assertBool "Expected exactly the viewport's rows and terminal columns" $
                length frame == TUI.height && all ((== columns) . sum . map Cell.columns) frame
    columns = TUI.width * 2
    rows = map (Text.concat . map (.glyph)) . TUI.renderFrame

partyTests :: TestTree
partyTests =
    testGroup
        "Party"
        [ testCase "Accept parties up to the game limit, including more than six members" $
            forM_ [1, 3, 9] $ \limit ->
                forM_ [[], replicate limit critter] $ \critters ->
                    assertRoundTrip initialState{GameState.maxPartySize = limit, GameState.party = Party critters}
        , testCase "Default an omitted party in current and legacy game files"
            $ forM_
                [ "{\"maps\":{\"overworld\":{\"tiles\":[]}},\"playerPosition\":{\"x\":0,\"y\":0},\"version\":1}"
                , "{\"playerPosition\":{\"x\":0,\"y\":0},\"version\":1,\"world\":{\"tiles\":[]}}"
                ]
            $ \encoded ->
                assertBool "Expected older files to start with an empty party and a limit of six" $
                    fmap (\state -> (state.party, state.maxPartySize)) (Aeson.eitherDecode @GameState encoded) == Right (Party [], 6)
        , testCase "Default omitted members to an empty party" $
            assertBool "Expected no members" $
                Aeson.eitherDecode @Party "{}" == Right (Party [])
        , testCase "Load an independently written fixture and preserve member order" $
            assertBool "Expected two distinct critters of the same species in the written order" $
                Aeson.eitherDecode fixture == Right configured
        , testCase "Load the game limit in current and legacy files" $
            forM_ ["\"maps\":{\"overworld\":{\"tiles\":[]}}", "\"world\":{\"tiles\":[]}"] $ \layout ->
                let encoded = "{" <> layout <> ",\"maxPartySize\":2,\"party\":" <> fixture <> ",\"playerPosition\":{\"x\":0,\"y\":0},\"version\":1}"
                 in assertBool "Expected the configured game limit and a party at that limit" $
                        fmap (\state -> (state.party, state.maxPartySize)) (Aeson.eitherDecode @GameState encoded) == Right (configured, 2)
        , testCase "Load a configured party from a complete game fixture" $
            let encoded = "{\"maps\":{\"overworld\":{\"tiles\":[]}},\"maxPartySize\":6,\"party\":" <> fixture <> ",\"playerPosition\":{\"x\":0,\"y\":0},\"version\":1}"
             in assertBool "Expected the configured party and game limit to load together" $
                    fmap (\state -> (state.party, state.maxPartySize)) (Aeson.eitherDecode @GameState encoded) == Right (configured, 6)
        , testCase "Preserve the party through movement and event replay" $
            let starting = initialState{GameState.maxPartySize = 2, GameState.party = configured}
                (moved, events) = Engine.step starting Input.Right
             in assertBool "Expected movement facts and replay to retain all party data" $
                    not (null events)
                        && moved.maxPartySize == 2
                        && moved.party == configured
                        && Save.apply starting events == moved
        , testCase "Reject game limits that are not positive integers" $
            forM_ ["\"maps\":{\"overworld\":{\"tiles\":[]}}", "\"world\":{\"tiles\":[]}"] $ \layout ->
                forM_ ["-1", "0", "1.5", "null", "\"six\""] $ \invalid ->
                    let encoded = "{" <> layout <> ",\"maxPartySize\":" <> invalid <> ",\"playerPosition\":{\"x\":0,\"y\":0},\"version\":1}"
                     in assertBool "Expected invalid game limits to fail loading" (isLeft (Aeson.eitherDecode @GameState encoded))
        , testCase "Reject invalid critters inside a party" $
            assertBool "Expected critter validation to apply to party members" $
                isLeft (Aeson.eitherDecode @Party (Aeson.encode (Party [critter{Critter.hitPoints = -1}])))
        , testCase "Reject parties above the game limit in current and legacy files" $
            forM_ ["\"maps\":{\"overworld\":{\"tiles\":[]}}", "\"world\":{\"tiles\":[]}"] $ \layout ->
                let encoded = "{" <> layout <> ",\"maxPartySize\":1,\"party\":" <> fixture <> ",\"playerPosition\":{\"x\":0,\"y\":0},\"version\":1}"
                 in assertBool "Expected a party larger than the game limit to fail loading" (isLeft (Aeson.eitherDecode @GameState encoded))
        , testCase "Round trip game state with a configured party" $
            assertRoundTrip initialState{GameState.maxPartySize = 2, GameState.party = configured}
        , testCase "Save the expected JSON structure" $
            assertBool "Expected only the ordered members in the party JSON" $
                Aeson.eitherDecode fixture == Right (Aeson.toJSON configured)
        ]
  where
    configured = Party [critter, critter{Critter.hitPoints = 7}]
    critter = basicCritter 20 5 20 "leafling"
    fixture = "{\"members\":[{\"hitPoints\":20,\"level\":5,\"maxHitPoints\":20,\"species\":\"leafling\"},{\"hitPoints\":7,\"level\":5,\"maxHitPoints\":20,\"species\":\"leafling\"}]}"

partyViewTests :: TestTree
partyViewTests =
    testGroup
        "Party view"
        [ testCase "Block gameplay through inputs and direct commands while open" $ do
            let opened = npcState{GameState.partyOpen = True}
            forM_ ([Input.Down, Input.Enter, Input.Left, Input.Right, Input.Up, Input.Character 'x'] <> map Input.Character "hjkl") $ \input ->
                assertBool "Expected gameplay inputs to leave the party view and world unchanged" $
                    Engine.commandFor opened input == Nothing && Engine.step opened input == (opened, [])
            forM_ [Command.AdvanceConversation, Command.ConfirmSelection, Command.EndConversation, Command.Interact, Command.OpenParty, Command.Select East, Command.Select North, Command.Select South, Command.Select West, Command.SelectNext, Command.SelectPrevious, Command.Walk East, Command.Walk North, Command.Walk South, Command.Walk West] $ \command ->
                assertBool "Expected direct commands to respect the open party view" (Engine.eventsFor opened command == [])
        , testCase "Keep conversation and party contexts separate" $
            let talking = npcState{GameState.conversation = Just (Conversation 0 (NpcId "guide"))}
             in assertBool "Expected p to be ignored during dialogue and quit to remain available" $
                    Engine.commandFor talking (Input.Character 'p') == Nothing
                        && Engine.step talking (Input.Character 'p') == (talking, [])
                        && Engine.eventsFor talking Command.OpenParty == []
                        && Engine.commandFor npcState{GameState.partyOpen = True} (Input.Character 'q') == Just Command.Quit
        , testCase "Load and round trip an open party view" $ do
            assertRoundTrip initialState{GameState.partyOpen = True}
            forM_ ["\"maps\":{\"overworld\":{\"tiles\":[]}}", "\"world\":{\"tiles\":[]}"] $ \layout ->
                let encoded = "{" <> layout <> ",\"partyOpen\":true,\"playerPosition\":{\"x\":0,\"y\":0},\"version\":1}"
                 in assertBool "Expected both game formats to preserve the open view" $
                        fmap (.partyOpen) (Aeson.eitherDecode @GameState encoded) == Right True
        , testCase "Open with p and close with p or Escape" $
            forM_ [Input.Character 'p', Input.Escape] $ \closeInput ->
                forM_ [initialState, npcState] $ \starting ->
                    let (opened, started) = Engine.step starting (Input.Character 'p')
                        (closed, ended) = Engine.step opened closeInput
                     in assertBool "Expected only the view state to change, with exploration restored on closing" $
                            Engine.commandFor starting (Input.Character 'p') == Just Command.OpenParty
                                && opened == starting{GameState.partyOpen = True}
                                && started == [Event.PartyOpened]
                                && Engine.commandFor opened closeInput == Just Command.CloseMenu
                                && closed == starting
                                && ended == [Event.PartyClosed]
                                && Engine.eventsFor closed Command.CloseMenu == []
                                && Engine.step closed Input.Up == Engine.step starting Input.Up
        , testCase "Replay view facts without checking their preconditions" $ do
            let talking = npcState{GameState.conversation = Just (Conversation 0 (NpcId "guide"))}
                opened = Engine.applyEvent talking Event.PartyOpened
                target = Location (MapId "missing") (Position (-3) 12)
            assertBool "PartyOpened should change only partyOpen, preserving the conversation" $
                opened == talking{GameState.partyOpen = True}
            assertBool "PartyClosed should change only partyOpen, preserving the conversation" $
                Engine.applyEvent opened Event.PartyClosed == talking
            assertBool "ConversationStarted should preserve partyOpen" $
                Engine.applyEvent npcState{GameState.partyOpen = True} (Event.ConversationStarted (NpcId "guide")) == talking{GameState.partyOpen = True}
            assertBool "PlayerWarped should preserve partyOpen while applying its existing map, position and conversation changes" $
                Engine.applyEvent opened (Event.PlayerWarped target) == npcState{GameState.partyOpen = True, GameState.playerMap = MapId "missing", GameState.playerPosition = Position (-3) 12}
        , testCase "Save and resume party view history" $
            withSaveFile $ \path ->
                forM_ [([Event.PartyOpened], initialState{GameState.partyOpen = True}), ([Event.PartyOpened, Event.PartyClosed], initialState)] $ \(history, expected) -> do
                    written <- Save.save path history
                    loaded <- Save.load path
                    assertBool "Expected view facts to survive disk and reproduce the open or closed state" $
                        written == Right () && loaded == Right history && fmap (Save.apply initialState) loaded == Right expected
        ]

saveTests :: TestTree
saveTests =
    testGroup
        "Save"
        [ testCase "Apply history in order without rechecking rules" $
            assertBool "Expected the final recorded position and original world" $
                Save.apply initialState history == initialState{GameState.playerPosition = Position (-4) 9}
        , testCase "Empty history preserves the starting state" $
            assertBool "Expected the original state" $
                Save.apply initialState [] == initialState
        , testCase "Load an independently written history" $
            withSaveFile $ \path ->
                forM_ ["\n", "\r\n"] $ \newline ->
                    forM_ ["", newline] $ \ending -> do
                        TextIO.writeFile path (Text.intercalate newline eventLines <> ending)
                        result <- Save.load path
                        assertBool "Expected ordered events with either newline convention and an optional final newline" (result == Right history)
        , testCase "Load legacy JSON-array histories" $
            withSaveFile $ \path -> do
                TextIO.writeFile path ("[" <> Text.intercalate "," eventLines <> "]")
                result <- Save.load path
                assertBool "Expected existing histories to remain readable" (result == Right history)
        , testCase "Read failures are returned" $
            withSaveFile $ \path -> do
                result <- Save.load (path <> "/history.json")
                assertBool "Expected a file error" (isLeft result)
        , testCase "Reject invalid histories" $
            withSaveFile $ \path ->
                forM_ ["{", "{}", "{\"type\":\"unknown\"}\n", "{\"position\":{\"x\":2,\"y\":1},\"type\":\"player-moved\"}\n{}\n", "\n", "[{\"type\":\"unknown\"}]"] $ \invalid -> do
                    TextIO.writeFile path invalid
                    result <- Save.load path
                    assertBool "Expected the entire history to be rejected" (isLeft result)
        , testCase "Round trip engine histories through disk" $
            withSaveFile $ \path -> do
                let (nextState, events) = Engine.step initialState Input.Right
                forM_ [(events, nextState), ([], initialState)] $ \(savedEvents, expectedState) -> do
                    written <- Save.save path savedEvents
                    loaded <- Save.load path
                    assertBool "Expected writing to succeed" (written == Right ())
                    assertBool "Expected the complete saved history" (loaded == Right savedEvents)
                    assertBool "Expected replay to reproduce the resulting state" (fmap (Save.apply initialState) loaded == Right expectedState)
        , testCase "Round trip an interrupted conversation" $
            withSaveFile $ \path -> do
                let (opened, started) = Engine.step npcState Input.Enter
                    (active, advanced) = Engine.step opened Input.Enter
                    conversationHistory = started <> advanced
                written <- Save.save path conversationHistory
                loaded <- Save.load path
                assertBool "Expected all conversation facts to survive JSON Lines and replay" $
                    written == Right ()
                        && loaded == Right conversationHistory
                        && fmap (Save.apply npcState) loaded == Right active
                        && conversationText active == Just "Visit my lab."
                        && Engine.step active Input.Enter == (npcState, [Event.ConversationEnded])
        , testCase "Write independently decodable events on separate lines" $
            withSaveFile $ \path ->
                forM_ [history, []] $ \savedEvents -> do
                    written <- Save.save path savedEvents
                    contents <- TextIO.readFile path
                    let decoded = traverse (Aeson.eitherDecodeStrict @Event.Event . TextEncoding.encodeUtf8) (Text.lines contents)
                    assertBool "Expected writing to succeed" (written == Right ())
                    assertBool "Expected one event per line in order" (decoded == Right savedEvents)
                    assertBool "Expected an empty file or a final newline" $
                        if null savedEvents then Text.null contents else Text.isSuffixOf "\n" contents
        , testCase "Write failures are returned" $
            withSaveFile $ \path -> do
                result <- Save.save (path <> "/history.json") history
                assertBool "Expected a file error" (isLeft result)
        ]
  where
    eventLines =
        [ "{\"position\":{\"x\":2,\"y\":1},\"type\":\"player-moved\"}"
        , "{\"position\":{\"x\":-4,\"y\":9},\"type\":\"player-moved\"}"
        ]
    history = map Event.PlayerMoved [Position 2 1, Position (-4) 9]

stateTests :: TestTree
stateTests =
    testGroup
        "State"
        [ testGroup
            "Decode errors"
            [ testCase "Fractional coordinate" $
                assertBool "Expected a parse error" $
                    isLeft (Aeson.eitherDecode @Position "{\"x\":1.5,\"y\":0}")
            , testCase "Invalid JSON" $
                assertBool "Expected a parse error" $
                    isLeft (Aeson.eitherDecode @GameState "{")
            , testCase "Invalid warp entrance direction" $
                assertBool "Expected an unknown movement direction to fail decoding" $
                    isLeft (Aeson.eitherDecode @Warp "{\"destination\":{\"mapId\":\"room\",\"position\":{\"x\":0,\"y\":1}},\"entranceDirection\":\"up\",\"source\":{\"x\":1,\"y\":1}}")
            , testCase "Missing coordinate" $
                assertBool "Expected a parse error" $
                    isLeft (Aeson.eitherDecode @Position "{\"x\":1}")
            , testCase "Missing save version" $
                assertBool "Expected a parse error" $
                    isLeft
                        ( Aeson.eitherDecode @GameState
                            "{\"playerPosition\":{\"x\":0,\"y\":0},\"world\":{\"tiles\":[]}}"
                        )
            , testCase "Non-string tile" $
                assertBool "Expected a parse error" $
                    isLeft (Aeson.eitherDecode @Tile "1")
            , testCase "Out-of-range coordinate" $
                assertBool "Expected a parse error" $
                    isLeft (Aeson.eitherDecode @Position "{\"x\":1e100,\"y\":0}")
            , testCase "Unknown tile" $
                assertBool "Expected a parse error" $
                    isLeft (Aeson.eitherDecode @Tile "\"lava\"")
            , testCase "Unknown world tile" $
                assertBool "Expected a parse error" $
                    isLeft (Aeson.eitherDecode @World "{\"tiles\":[[\"lava\"]]}")
            , testCase "Unsupported save version" $
                assertBool "Expected a parse error" $
                    isLeft
                        ( Aeson.eitherDecode @GameState
                            "{\"playerPosition\":{\"x\":0,\"y\":0},\"version\":2,\"world\":{\"tiles\":[]}}"
                        )
            ]
        , testGroup
            "JSON structure"
            [ testCase "Encode and decode ledge directions" $
                forM_ [(East, "\"ledge-east\""), (North, "\"ledge-north\""), (South, "\"ledge-south\""), (West, "\"ledge-west\"")] $ \(direction, encoded) ->
                    assertBool "Expected an explicit direction in the tile JSON" $
                        Aeson.eitherDecode encoded == Right (Ledge direction)
                            && Aeson.encode (Ledge direction) == encoded
            , testCase "Encode and decode warp entrance directions"
                $ forM_
                    [ ("{\"destination\":{\"mapId\":\"room\",\"position\":{\"x\":0,\"y\":1}},\"source\":{\"x\":1,\"y\":1}}", Nothing)
                    , ("{\"destination\":{\"mapId\":\"room\",\"position\":{\"x\":0,\"y\":1}},\"entranceDirection\":\"west\",\"source\":{\"x\":1,\"y\":1}}", Just West)
                    ]
                $ \(encoded, allowed) ->
                    let expected = Warp (Location (MapId "room") (Position 0 1)) allowed (Position 1 1)
                     in assertBool "Expected an optional entrance direction in the warp JSON" $
                            Aeson.eitherDecode encoded == Right expected
                                && Aeson.eitherDecode encoded == Right (Aeson.toJSON expected)
            , testCase "Load an independently written fixture" $
                assertBool "Expected the fixture's state" $
                    Aeson.eitherDecode fixture == Right state
            , testCase "Load legacy single-map files" $
                let encoded = "{\"npcs\":{\"guide\":{\"dialogue\":[\"Hello!\"],\"location\":{\"mapId\":\"overworld\",\"position\":{\"x\":1,\"y\":0}},\"name\":\"Guide\"}},\"playerPosition\":{\"x\":0,\"y\":0},\"version\":1,\"world\":{\"tiles\":[[\"ground\",\"grass\"]]}}"
                    expected = (gameState (Position 0 0) (filledWorld [[Ground, Grass]])){GameState.npcs = Map.singleton (NpcId "guide") (Npc ["Hello!"] [] (Location (MapId "overworld") (Position 1 0)) "Guide")}
                 in assertBool "Expected the old world and NPC positions to belong to the overworld" $
                        Aeson.eitherDecode encoded == Right expected
            , testCase "Load missing cells" $
                assertBool "Expected JSON null to represent an absent cell" $
                    Aeson.eitherDecode @World "{\"tiles\":[[null,\"ground\",null]]}" == Right (World [[Nothing, Just Ground, Nothing]] [])
            , testCase "Load NPCs and an active conversation" $
                let encoded = "{\"conversation\":{\"lineIndex\":1,\"npcId\":\"guide\"},\"npcs\":{\"guide\":{\"dialogue\":[\"Hello!\",\"Visit my lab.\"],\"location\":{\"mapId\":\"overworld\",\"position\":{\"x\":1,\"y\":0}},\"name\":\"Guide\"}},\"playerDirection\":\"east\",\"playerPosition\":{\"x\":0,\"y\":0},\"version\":1,\"maps\":{\"overworld\":{\"tiles\":[[\"ground\",\"grass\"]]}}}"
                    expected =
                        (gameState (Position 0 0) (filledWorld [[Ground, Grass]]))
                            { GameState.conversation = Just (Conversation 1 (NpcId "guide"))
                            , GameState.npcs = Map.singleton (NpcId "guide") (Npc ["Hello!", "Visit my lab."] [] (Location (MapId "overworld") (Position 1 0)) "Guide")
                            , GameState.playerDirection = East
                            }
                 in assertBool "Expected stable NPC object keys, facing, dialogue and progress" $
                        Aeson.eitherDecode encoded == Right expected
                            && Aeson.eitherDecode encoded == Right (Aeson.toJSON expected)
            , testCase "Save the expected JSON structure" $
                assertBool "Expected the fixture's JSON fields" $
                    Aeson.eitherDecode fixture == Right (Aeson.toJSON state)
            ]
        , testGroup
            "Round trips"
            [ testCase "Conversation" $ forM_ [Conversation 0 (NpcId "guide"), Conversation 1 (NpcId "guide")] assertRoundTrip
            , testCase "Direction" $ forM_ [East, North, South, West] assertRoundTrip
            , testCase "GameState" $ forM_ [initialState, npcState, npcState{GameState.conversation = Just (Conversation 1 (NpcId "guide"))}, warpState, fst (Engine.step warpState Input.Up)] assertRoundTrip
            , testCase "Location" $ forM_ [Location (MapId "overworld") (Position 1 2), Location (MapId "room") (Position 0 1)] assertRoundTrip
            , testCase "MapId" $ forM_ [MapId "overworld", MapId "room"] assertRoundTrip
            , testCase "Npc" $ forM_ [Npc [] [] (Location (MapId "overworld") (Position 0 0)) "Guide", Npc ["Hello!", "Visit my lab."] [] (Location (MapId "overworld") (Position 2 1)) "Guide"] assertRoundTrip
            , testCase "NpcId" $ forM_ [NpcId "guide", NpcId "博士"] assertRoundTrip
            , testCase "Position" $
                forM_ [Position 0 0, Position (-1) 2, Position minBound maxBound] assertRoundTrip
            , testCase "Tile" $
                forM_ [Door, Fence, Flowers, Grass, Ground, Arena, House, Laboratory, Ledge East, Ledge North, Ledge South, Ledge West, Shop, Path, Clinic, Rock, Sand, Sign, Stairs, TallGrass, Tree, Wall, Water] assertRoundTrip
            , testCase "Warp" $
                forM_ [Nothing, Just East, Just North, Just South, Just West] $ \allowed ->
                    assertRoundTrip (Warp (Location (MapId "room") (Position 0 1)) allowed (Position 1 2))
            , testCase "World" $
                forM_ [filledWorld [], filledWorld [[]], filledWorld [[Ground], [], [Water, Wall]], World [[Nothing, Just Ground, Nothing]] [], world warpState] assertRoundTrip
            ]
        , testGroup
            "World coordinates"
            [ testCase "Bottom-left origin with y increasing upwards" $
                assertBool "Expected columns to run rightwards and rows upwards" $
                    map (tileAt sampleWorld) [Position 0 0, Position 0 1, Position 1 0, Position 1 1]
                        == map Just [Ground, TallGrass, Water, Wall]
            , testCase "Missing cells" $
                assertBool "Expected empty worlds and missing cells to return Nothing" $
                    tileAt (filledWorld []) (Position 0 0) == Nothing
                        && tileAt (filledWorld [[]]) (Position 0 0) == Nothing
                        && tileAt (filledWorld [[Ground], []]) (Position 0 0) == Nothing
                        && tileAt (filledWorld [[Ground], []]) (Position 0 1) == Just Ground
                        && tileAt (World [[Nothing, Just Ground]] []) (Position 0 0) == Nothing
                        && tileAt (World [[Nothing, Just Ground]] []) (Position 1 0) == Just Ground
            , testCase "Outside the world"
                $ forM_
                    [ Position (-1) 0
                    , Position 0 (-1)
                    , Position 0 2
                    , Position 2 0
                    , Position maxBound maxBound
                    , Position minBound minBound
                    ]
                $ \position -> assertBool "Expected Nothing outside the world" (tileAt sampleWorld position == Nothing)
            ]
        ]
  where
    fixture =
        "{\"playerPosition\":{\"x\":2,\"y\":1},\"version\":1,\"maps\":{\"overworld\":{\"tiles\":[[\"ground\",\"wall\"],[\"water\",\"tall-grass\"]]}}}"
    sampleWorld = filledWorld [[TallGrass, Wall], [Ground, Water]]
    state = gameState (Position 2 1) (filledWorld [[Ground, Wall], [Water, TallGrass]])

tests :: TestTree
tests = testGroup "Critters" [BattleEventTests.tests, BattlePersistenceTests.tests, BattleTests.tests, conditionTests, conversationTests, critterTests, engineTests, eventTests, inputTests, InteractionTests.tests, partyRenderTests, partyTests, partyViewTests, PlaybackTests.tests, saveTests, SceneTests.tests, SchemaTests.tests, stateTests, TurnEventTests.tests, tuiTests, warpTests]

tuiTests :: TestTree
tuiTests =
    testGroup
        "TUI"
        [ testCase "Keep the player centered at and beyond map edges"
            $ forM_
                [ Position 0 0
                , Position 0 (TUI.height * 2 - 1)
                , Position TUI.width TUI.height
                , Position (TUI.width * 2 - 1) 0
                , Position (TUI.width * 2 - 1) (TUI.height * 2 - 1)
                , Position (-5) (-8)
                ]
            $ \position ->
                forM_ [largeWorld, filledWorld [[Ground]], filledWorld []] $ \layout ->
                    assertBool "Expected the player at the same central viewport cell regardless of map bounds" $
                        tile (view (gameState position layout)) (0, 0) == "🧑"
        , testCase "Draw one marker per building footprint" $
            forM_ [(Arena, "🥊"), (House, "🏠"), (Laboratory, "🔬"), (Shop, "🏪"), (Clinic, "🏥")] $ \(building, marker) ->
                let layout = filledWorld [[building, building], [building, Door]]
                    drawing = TUI.render (gameState (Position (-1) (-1)) layout)
                 in assertBool "Expected one building marker and its separate door" $
                        Text.count marker drawing == 1 && Text.count "🚪" drawing == 1
        , testCase "Draw stationary NPCs over their terrain" $
            assertBool "Expected the NPC and player at their separate ground positions" $
                tile (view npcState) (1, 0) == "🧓"
                    && tile (view npcState) (0, 0) == "🧑"
        , testCase "Empty and ragged worlds retain a fixed viewport" $
            forM_ [filledWorld [], filledWorld [[]], filledWorld [[Wall], [], [Ground, Water]]] $ \layout -> do
                let state = gameState (Position minBound maxBound) layout
                    rows = view state
                assertBool "Expected the configured rows and one wide glyph per floor cell" $
                    length rows == TUI.height
                        && all ((== TUI.width) . Text.length) rows
                        && all ((== TUI.width * 2) . sum . map Cell.columns) (TUI.renderFrame state)
                assertBool "Expected even out-of-world players to remain centered" $
                    tile rows (0, 0) == "🧑" && sum (map (Text.count "🧑") rows) == 1
        , testCase "Movement scrolls terrain while keeping the player centered" $
            let layout = filledWorld [[Ground, Ground, Wall, Ground, Ground], replicate 5 Ground, replicate 5 Ground, replicate 5 Ground, replicate 5 Ground]
                state = gameState (Position 2 2) layout
                (nextState, _) = Engine.step state Input.Up
             in assertBool "Expected terrain to shift south as the player walks north" $
                    tile (view state) (0, 0) == "🧑"
                        && tile (view nextState) (0, 0) == "🧑"
                        && tile (view state) (0, 2) == "🧱"
                        && tile (view nextState) (0, 1) == "🧱"
                        && tile (view nextState) (0, -1) == "⬜"
        , testCase "Render ledge directions" $
            let rows = view (gameState (Position (-1) (-1)) (filledWorld [[Ledge East, Ledge North, Ledge South, Ledge West]]))
             in assertBool "Expected arrows showing the permitted crossing direction" $
                    Text.concat [tile rows (column, 1) | column <- [1 .. 4]] == "⏩🔺🔻⏪"
        , testCase "Render dialogue in two white rows within the viewport" $
            forM_ [(0, "Hello!"), (1, "Visit my lab.")] $ \(index, expected) ->
                let active = npcState{GameState.conversation = Just (Conversation index (NpcId "guide"))}
                    drawing = view active
                 in assertBool "Expected a fixed-height viewport with two padded white rows and a centered player" $
                        length drawing == TUI.height
                            && take (TUI.height - 2) drawing == take (TUI.height - 2) (view npcState)
                            && drop (TUI.height - 2) drawing == [styled (Text.justifyLeft columns ' ' expected), styled (Text.replicate columns " ")]
                            && all ((== columns) . sum . map Cell.columns) (TUI.renderFrame active)
                            && tile drawing (0, 0) == "🧑"
        , testCase "Redraw changed colors even when characters match"
            $ forM_
                [ (False, True, styled " ")
                , (True, False, " ")
                ]
            $ \(previous, next, expected) ->
                assertBool "Expected a color-only cell change to be drawn and its style reset" $
                    TUI.redraw [[Cell " " previous]] [[Cell " " next]] == "\ESC[1;1H" <> expected
        , testCase "Redraw only changed cells" $
            assertBool "Expected individual cell replacements at their row and column, without clearing other content" $
                TUI.redraw
                    [[Cell "a" False, Cell "b" False], [Cell "c" False, Cell "d" False]]
                    [[Cell "a" False, Cell "x" False], [Cell "c" False, Cell "y" False]]
                    == "\ESC[1;2Hx\ESC[2;2Hy"
        , testCase "Position emoji updates after wide padding" $
            assertBool "Expected the changed emoji after two full-width cells to start in terminal column five" $
                TUI.redraw
                    [[Cell "\x3000" False, Cell "🧱" False, Cell "🪜" False]]
                    [[Cell "\x3000" False, Cell "🧱" False, Cell "⬜" False]]
                    == "\ESC[1;5H⬜"
        , testCase "Redraw shifted cells when glyph widths change"
            $ forM_
                [ (["界", "b", "c"], ["a", "b", "c", " "], "\ESC[1;1Ha\ESC[1;2Hb\ESC[1;3Hc\ESC[1;4H ")
                , (["a", "b", "c", " "], ["界", "b", "c"], "\ESC[1;1H界\ESC[1;3Hb\ESC[1;4Hc")
                ]
            $ \(previous, next, expected) ->
                assertBool "Expected shifted glyphs to move even when their text is unchanged" $
                    TUI.redraw [map (\value -> Cell value False) previous] [map (\value -> Cell value False) next] == expected
        , testCase "Respect line breaks and discard further dialogue rows" $
            assertBool "Expected only the first two explicit lines, with colors reset after each" $
                dialogueRows "First line\r\nSecond line\nHidden third line"
                    == [styled (Text.justifyLeft columns ' ' "First line"), styled (Text.justifyLeft columns ' ' "Second line")]
        , testCase "Skip redraws for identical frames" $
            forM_ [initialState{GameState.partyOpen = True}, npcState, npcState{GameState.conversation = Just (Conversation 0 (NpcId "guide"))}] $ \state ->
                let rows = TUI.renderFrame state
                 in assertBool "Expected unchanged frames to produce no terminal output" $
                        Text.null (TUI.redraw rows rows)
        , testCase "Move whole words to the next dialogue row" $
            assertBool "Expected a word that does not fit to start on the next row" $
                dialogueRows (Text.replicate (columns - 3) "a" <> " word")
                    == [styled (Text.justifyLeft columns ' ' (Text.replicate (columns - 3) "a")), styled (Text.justifyLeft columns ' ' "word")]
        , testCase "Wrap dialogue across two rows and clip overflow" $
            assertBool "Expected full-width text rows and no third-row overflow" $
                dialogueRows (Text.replicate columns "a" <> Text.replicate columns "b" <> "DISCARD")
                    == [styled (Text.replicate columns "a"), styled (Text.replicate columns "b")]
        , testCase "Wrap wide and combining characters by terminal columns"
            $ forM_
                [
                    ( Text.replicate (columns `div` 2) "界" <> Text.replicate columns "b" <> "DISCARD"
                    , [styled (Text.replicate (columns `div` 2) "界"), styled (Text.replicate columns "b")]
                    )
                ,
                    ( Text.replicate (columns - 1) "a" <> "界z"
                    , [styled (Text.replicate (columns - 1) "a" <> " "), styled ("界z" <> Text.replicate (columns - 3) " ")]
                    )
                ,
                    ( Text.replicate columns "e\x0301" <> Text.replicate columns "b" <> "DISCARD"
                    , [styled (Text.replicate columns "e\x0301"), styled (Text.replicate columns "b")]
                    )
                ]
            $ \(message, expected) ->
                assertBool "Expected complete Unicode characters within the two-row column budget" $
                    dialogueRows message == expected
        , testCase "Render missing cells as padding" $
            let rows = view (gameState (Position (-1) (-1)) (World [[Nothing, Just Ground, Nothing]] []))
             in assertBool "Expected missing cells on either side of real terrain to remain blank" $
                    Text.concat [tile rows (column, 1) | column <- [1 .. 3]] == "\x3000⬜\x3000"
        , testCase "Render tiles north-up with the player over the terrain" $
            let rows = view (gameState (Position 0 0) (filledWorld [[TallGrass, Wall, Ground], [Ground, Water, Wall]]))
             in assertBool "Expected north-up terrain around the centered player and blanks beyond every map edge" $
                    Text.concat [tile rows (column, 1) | column <- [0 .. 2]] == "🌿🧱⬜"
                        && Text.concat [tile rows (column, 0) | column <- [0 .. 2]] == "🧑🌊🧱"
                        && tile rows (-1, 0) == "\x3000"
                        && tile rows (0, -1) == "\x3000"
                        && tile rows (3, 0) == "\x3000"
                        && tile rows (0, TUI.height `div` 2) == "\x3000"
        ]
  where
    columns = TUI.width * 2
    dialogueRows message =
        drop (TUI.height - 2) $
            view
                npcState
                    { GameState.conversation = Just (Conversation 0 (NpcId "guide"))
                    , GameState.npcs = Map.adjust (\npc -> npc{Npc.dialogue = [message]}) (NpcId "guide") npcState.npcs
                    }
    largeWorld = filledWorld (replicate (TUI.height * 2) (replicate (TUI.width * 2) Ground))
    styled line = "\ESC[30;107m" <> line <> "\ESC[0m"
    tile rows (column, row) = Text.take 1 $ Text.drop (TUI.width `div` 2 + column) (rows !! (TUI.height `div` 2 - row))
    view = Text.lines . TUI.render

warpState :: GameState
warpState =
    (gameState (Position 1 1) (filledWorld []))
        { GameState.maps =
            Map.fromList
                [ (MapId "overworld", (filledWorld [[Ground, Path, Ground], replicate 3 Ground, replicate 3 Ground]){World.warps = [Warp (Location (MapId "room") (Position 0 1)) Nothing (Position 1 2)]})
                , (MapId "room", (filledWorld [[Ground, Ground], [Ground, Water]]){World.warps = [Warp (Location (MapId "overworld") (Position 1 1)) Nothing (Position 0 0)]})
                ]
        }

warpTests :: TestTree
warpTests =
    testGroup
        "Warp"
        [ testCase "Apply warp facts without rechecking their destinations" $
            let target = Location (MapId "missing") (Position (-3) 12)
                active = warpState{GameState.conversation = Just (Conversation 0 (NpcId "missing"))}
             in assertBool "Expected applying a fact to update both map and position, closing dialogue" $
                    Engine.applyEvent active (Event.PlayerWarped target)
                        == warpState{GameState.playerMap = MapId "missing", GameState.playerPosition = Position (-3) 12}
        , testCase "Block invalid warp destinations" $ do
            forM_ [Nothing, Just Wall, Just Water, Just Tree] $ \landing ->
                let current = warpState{GameState.maps = Map.adjust (\layout -> layout{World.tiles = [[landing], [Just Ground]]}) (MapId "room") warpState.maps}
                 in assertBool "Expected no movement into an unusable destination" $
                        Engine.step current Input.Up == (current, [])
            let absent = warpState{GameState.maps = Map.delete (MapId "room") warpState.maps}
                outside = warpState{GameState.maps = Map.adjust (\layout -> layout{World.tiles = []}) (MapId "room") warpState.maps}
                occupied = warpState{GameState.npcs = Map.singleton (NpcId "inside") (Npc [] [] (Location (MapId "room") (Position 0 1)) "Inside")}
            forM_ [absent, outside, occupied] $ \current ->
                assertBool "Expected missing maps, missing cells and NPCs to block arrival" $
                    Engine.step current Input.Up == (current, [])
        , testCase "Do not chain reciprocal warps on arrival" $
            let current = warpState{GameState.maps = Map.adjust (\layout -> layout{World.warps = [Warp (Location (MapId "overworld") (Position 1 1)) Nothing (Position 0 1)]}) (MapId "room") warpState.maps}
                (arrived, events) = Engine.step current Input.Up
             in assertBool "Expected one transition per input even when the arrival cell has a return warp" $
                    arrived.playerMap == MapId "room"
                        && events == [Event.PlayerMoved (Position 1 2), Event.PlayerWarped (Location (MapId "room") (Position 0 1))]
        , testCase "Enforce required entrance directions" $
            forM_ [East, North, South, West] $ \allowed ->
                forM_
                    [ (East, Input.Right, Position 0 1)
                    , (North, Input.Up, Position 1 0)
                    , (South, Input.Down, Position 1 2)
                    , (West, Input.Left, Position 2 1)
                    ]
                    $ \(direction, input, origin) ->
                        let current = (withEntrance (Just allowed)){GameState.playerDirection = direction, GameState.playerPosition = origin}
                            expected =
                                if direction == allowed
                                    then (current{GameState.playerMap = MapId "room", GameState.playerPosition = Position 0 1}, [Event.PlayerMoved (Position 1 1), Event.PlayerWarped (Location (MapId "room") (Position 0 1))])
                                    else (current, [])
                         in assertBool "Expected stairs to admit only their configured movement direction" $
                                Engine.step current input == expected
        , testCase "Ignore NPCs in other maps" $
            let current = warpState{GameState.npcs = Map.singleton (NpcId "outside") (Npc ["Hello!"] [] (Location (MapId "overworld") (Position 0 1)) "Outside")}
                (arrived, _) = Engine.step current Input.Up
             in assertBool "Expected unrelated NPCs neither to block arrival nor appear in the destination map" $
                    arrived.playerMap == MapId "room"
                        && Text.count "🧓" (TUI.render current) == 1
                        && Text.count "🧓" (TUI.render arrived) == 0
                        && snd (Engine.step arrived Input.Enter) == []
        , testCase "Preserve turning when an entrance direction is rejected" $
            let current = (withEntrance (Just East)){GameState.playerDirection = South, GameState.playerPosition = Position 1 0}
             in assertBool "Expected only the facing change when approaching stairs from the wrong direction" $
                    Engine.step current Input.Up == (current{GameState.playerDirection = North}, [Event.PlayerTurned North])
        , testCase "Render the destination map" $
            let (arrived, _) = Engine.step warpState Input.Up
                rows = Text.lines (TUI.render arrived)
             in assertBool "Expected only the active map's terrain, player and padding" $
                    Text.take 3 (Text.drop (TUI.width `div` 2) (rows !! (TUI.height `div` 2))) == "🧑⬜\x3000"
                        && Text.take 3 (Text.drop (TUI.width `div` 2) (rows !! (TUI.height `div` 2 + 1))) == "⬜🌊\x3000"
        , testCase "Replay map transitions through the save file" $
            withSaveFile $ \path -> do
                let (arrived, events) = Engine.step warpState Input.Up
                written <- Save.save path events
                loaded <- Save.load path
                assertBool "Expected the active map and position to survive JSON Lines and replay" $
                    written == Right () && fmap (Save.apply warpState) loaded == Right arrived
        , testCase "Return through an interior exit" $
            let (inside, _) = Engine.step warpState Input.Up
                (outside, events) = Engine.step inside Input.Down
             in assertBool "Expected a return warp to restore the exterior location" $
                    outside == warpState{GameState.playerDirection = South}
                        && events == [Event.PlayerTurned South, Event.PlayerMoved (Position 0 0), Event.PlayerWarped (Location (MapId "overworld") (Position 1 1))]
        , testCase "Unrestricted warps accept every direction"
            $ forM_
                [ (East, Input.Right, Position 0 1)
                , (North, Input.Up, Position 1 0)
                , (South, Input.Down, Position 1 2)
                , (West, Input.Left, Position 2 1)
                ]
            $ \(direction, input, origin) ->
                let current = (withEntrance Nothing){GameState.playerDirection = direction, GameState.playerPosition = origin}
                 in assertBool "Expected warps without a required direction to retain their behavior" $
                        Engine.step current input
                            == (current{GameState.playerMap = MapId "room", GameState.playerPosition = Position 0 1}, [Event.PlayerMoved (Position 1 1), Event.PlayerWarped (Location (MapId "room") (Position 0 1))])
        , testCase "Warp from ordinary floor tiles" $
            let (arrived, events) = Engine.step warpState Input.Up
             in assertBool "Expected a Path tile to trigger a transition independently of doors" $
                    tileAt (world warpState) (Position 1 2) == Just Path
                        && arrived == warpState{GameState.playerMap = MapId "room", GameState.playerPosition = Position 0 1}
                        && events == [Event.PlayerMoved (Position 1 2), Event.PlayerWarped (Location (MapId "room") (Position 0 1))]
        ]
  where
    withEntrance :: Maybe Direction -> GameState
    withEntrance allowed =
        warpState
            { GameState.maps =
                Map.insert
                    (MapId "overworld")
                    ((filledWorld [replicate 3 Ground, [Ground, Stairs, Ground], replicate 3 Ground]){World.warps = [Warp (Location (MapId "room") (Position 0 1)) allowed (Position 1 1)]})
                    warpState.maps
            }

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
