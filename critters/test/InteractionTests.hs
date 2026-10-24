module InteractionTests (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Critters.Command qualified as Command
import Critters.Engine qualified as Engine
import Critters.Event qualified as Event
import Critters.Input qualified as Input
import Critters.Input.Direction (Direction (..))
import Critters.Interface.TUI qualified as TUI
import Critters.Interface.TUI.Cell qualified as Cell
import Critters.Save qualified as Save
import Critters.State (ActiveInteraction (..), Choice (..), Condition (..), Critter (..), Effect (..), FlagId (..), GameState (..), Interaction (..), Location (..), MapId (..), Npc (..), NpcId (..), Object (..), ObjectId (..), Party (..), Position (..), Tile (..), Warp (..), World (..), basicCritter, initialState, interactionText, objectAt)
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

assertRoundTrip :: (Eq a, Aeson.FromJSON a, Aeson.ToJSON a) => a -> Assertion
assertRoundTrip value = assertBool "Expected JSON to preserve the value" (Aeson.eitherDecode (Aeson.encode value) == Right value)

capsuleId :: ObjectId
capsuleId = ObjectId "gift-box"

claimed :: FlagId
claimed = FlagId "gift.claimed"

codecTests :: TestTree
codecTests =
  testGroup
    "Codecs"
    [ testCase "Decode independently written effects"
        $ forM_
          [ ("{\"type\":\"give-critter\",\"critter\":{\"hitPoints\":8,\"level\":2,\"maxHitPoints\":10,\"species\":\"leafling\"}}", GiveCritter critter),
            ("{\"type\":\"remove-object\",\"objectId\":\"gift-box\"}", RemoveObject capsuleId),
            ("{\"type\":\"set-flag\",\"flag\":\"gift.claimed\",\"value\":true}", SetFlag claimed True)
          ]
        $ \(encoded, expected) ->
          assertBool "Expected the requested effect and its JSON structure" $
            Aeson.eitherDecode encoded == Right expected && Aeson.eitherDecode encoded == Right (Aeson.toJSON expected),
      testCase "Decode defaults and scripted NPCs without legacy dialogue" $ do
        assertBool "Expected an empty interaction" (Aeson.eitherDecode @Interaction "{}" == Right (Interaction [] [] [] []))
        assertBool "Expected a choice with no effects" (Aeson.eitherDecode @Choice "{\"label\":\"No\"}" == Right (Choice [] "No"))
        assertBool "Expected a scripted NPC without a dialogue field" $
          Aeson.eitherDecode @Npc "{\"interactions\":[{\"dialogue\":[\"Hello.\"]}],\"location\":{\"mapId\":\"test\",\"position\":{\"x\":1,\"y\":2}}}"
            == Right (Npc [] [Interaction [] [] ["Hello."] []] (Location (MapId "test") (Position 1 2))),
      testCase "Reject malformed effects and nested conditions" $ do
        forM_ ["{\"type\":\"give-critter\"}", "{\"type\":\"remove-object\"}", "{\"type\":\"set-flag\",\"flag\":\"x\",\"value\":null}", "{\"type\":\"starter\"}"] $ \encoded ->
          assertBool "Expected invalid effects to fail decoding" (isLeft (Aeson.eitherDecode @Effect encoded))
        assertBool "Expected the nested condition to use type flag" $
          isLeft (Aeson.eitherDecode @Interaction "{\"conditions\":[{\"type\":\"flag-is\",\"flag\":\"x\",\"value\":true}]}"),
      testCase "Round trip scripts, objects, active views and runtime events" $ do
        assertRoundTrip gift
        assertRoundTrip capsuleId
        assertRoundTrip (Object "🔴" [gift] (Location (MapId "test") (Position 2 1)))
        assertRoundTrip permitted
        forM_ [Nothing, Just 0, Just 1] $ \selected -> do
          let active = ActiveInteraction 0 gift selected
          assertRoundTrip active
          assertRoundTrip permitted{activeInteraction = Just active}
          assertRoundTrip (Event.InteractionStarted active)
        forM_ [Event.CritterReceived critter, Event.FlagChanged claimed False, Event.InteractionAdvanced 1, Event.InteractionChoiceOpened, Event.InteractionChoiceSelected 1, Event.InteractionEnded, Event.ObjectRemoved capsuleId] assertRoundTrip
    ]

critter :: Critter
critter = basicCritter 8 2 10 "leafling"

effectTests :: TestTree
effectTests =
  testGroup
    "Effects"
    [ testCase "Produce independent facts in effect order"
        $ assertBool "Expected a critter, object removal and flag change"
        $ Engine.eventsForEffects permitted [GiveCritter critter, RemoveObject capsuleId, SetFlag claimed True]
          == Right [Event.CritterReceived critter, Event.ObjectRemoved capsuleId, Event.FlagChanged claimed True],
      testCase "Validate cumulative party capacity before returning any facts"
        $ assertBool "Expected the second gift to reject the entire batch"
        $ isLeft (Engine.eventsForEffects permitted{maxPartySize = 1} [SetFlag claimed True, GiveCritter critter, GiveCritter critter, RemoveObject capsuleId]),
      testCase "Reject a later missing object without granting an earlier gift"
        $ assertBool "Expected no successful prefix of the effects"
        $ isLeft (Engine.eventsForEffects permitted [GiveCritter critter, RemoveObject (ObjectId "missing"), SetFlag claimed True]),
      testCase "Apply recorded facts without rechecking effect preconditions" $
        let starting = permitted{maxPartySize = 1, objects = Map.empty, party = Party [critter]}
            received = Save.apply starting [Event.CritterReceived critter, Event.ObjectRemoved capsuleId, Event.FlagChanged claimed True]
         in assertBool "Expected replay to apply the facts and preserve unrelated state" $
              received == starting{flags = Map.insert claimed True (flags starting), party = Party [critter, critter]}
    ]

exampleState :: GameState
exampleState =
  initialState
    { maps = Map.singleton (MapId "workshop") (World (replicate 10 (replicate 12 (Just Ground))) []),
      npcs =
        Map.singleton
          (NpcId "guide")
          ( Npc
              []
              [ Interaction [] [Flag (FlagId "lab.critter-claimed") True] ["Take good care of your new companion!"] [],
                Interaction [] [Flag (FlagId "guide.offer-made") True] ["Go ahead and choose one of the three critters."] [],
                Interaction [] [] ["Welcome to my lab!", "You may choose one of these three critters."] [SetFlag (FlagId "guide.offer-made") True]
              ]
              (Location (MapId "workshop") (Position 5 6))
          ),
      objects = Map.fromList (map capsule [(7, "leafling"), (8, "emberling"), (9, "brookling")]),
      playerMap = MapId "workshop"
    }
  where
    capsule (column, name) =
      let identifier = ObjectId ("lab-" <> name)
       in ( identifier,
            Object
              "🔴"
              [ Interaction [] [Flag (FlagId "lab.critter-claimed") True] ["You have already chosen your companion."] [],
                Interaction [] [Flag (FlagId "guide.offer-made") False] ["These belong to Guide. You should ask him first."] [],
                Interaction
                  [Choice [GiveCritter (basicCritter 20 5 20 name), RemoveObject identifier, SetFlag (FlagId "lab.critter-claimed") True] "Yes", Choice [] "No"]
                  []
                  ["This capsule contains " <> Text.toTitle name <> ". Choose it?"]
                  []
              ]
              (Location (MapId "workshop") (Position column 6))
          )

exampleTests :: TestTree
exampleTests =
  testGroup
    "Configured offers"
    [ testCase "Each configured critter can be chosen after Guide's offer" $ do
        let base = exampleState
        forM_ [(7, "leafling"), (8, "emberling"), (9, "brookling")] $ \(column, name) -> do
          let starting = base{playerDirection = North, playerMap = MapId "workshop", playerPosition = Position 5 5}
              inputs = replicate 3 Input.Enter <> replicate (column - 5) Input.Right <> [Input.Up, Input.Enter, Input.Enter]
              (finished, history) = play starting inputs
              identifier = ObjectId ("lab-" <> name)
          assertBool "Expected the selected critter, its removed capsule, both story flags, and a closed interaction" $
            members (party finished) == [basicCritter 20 5 20 name]
              && not (Map.member identifier (objects finished))
              && Map.size (objects finished) == 2
              && Map.lookup (FlagId "guide.offer-made") (flags finished) == Just True
              && Map.lookup (FlagId "lab.critter-claimed") (flags finished) == Just True
              && activeInteraction finished == Nothing
              && Save.apply starting history == finished
          let (atGuide, _) = play finished (replicate (column - 5) Input.Left <> [Input.Up, Input.Enter])
          assertBool "Expected Guide's follow-up branch" (interactionText atGuide == Just "Take good care of your new companion!"),
      testCase "A capsule cannot be claimed before speaking to Guide" $ do
        let base = exampleState
        let starting = base{playerDirection = North, playerMap = MapId "workshop", playerPosition = Position 7 5}
            (opened, _) = Engine.step starting Input.Enter
            (closed, _) = Engine.step opened Input.Enter
        assertBool "Expected the ask-Guide message and no party, object or flag changes" $
          interactionText opened == Just "These belong to Guide. You should ask him first."
            && party closed == party starting
            && objects closed == objects starting
            && flags closed == flags starting,
      testCase "One claim prevents taking either remaining capsule" $ do
        let base = exampleState
        let starting = base{playerDirection = North, playerMap = MapId "workshop", playerPosition = Position 5 5}
            (received, _) = play starting (replicate 3 Input.Enter <> [Input.Right, Input.Right, Input.Up, Input.Enter, Input.Enter])
            (other, _) = play received [Input.Right, Input.Up, Input.Enter]
        assertBool "Expected the shared flag to select the already-chosen dialogue" $
          interactionText other == Just "You have already chosen your companion."
            && length (members (party other)) == 1
            && Map.size (objects other) == 2
    ]

gift :: Interaction
gift =
  Interaction
    [Choice [GiveCritter critter, RemoveObject capsuleId, SetFlag claimed True] "Yes", Choice [] "No"]
    []
    ["Take this critter?"]
    []

interactionTests :: TestTree
interactionTests =
  testGroup
    "Execution"
    [ testCase "Choose the first branch whose conditions all hold" $
        let first = Interaction [] [Flag permission True, Flag claimed False] ["Allowed"] []
            fallback = Interaction [] [] ["Fallback"] []
         in assertBool "Expected priority, conjunction, missing-false semantics and no-match handling" $
              Engine.chooseInteraction permitted [first, fallback] == Just first
                && Engine.chooseInteraction scene [first, fallback] == Just fallback
                && Engine.chooseInteraction scene [first] == Nothing,
      testCase "Complete dialogue effects only after the final line" $
        let (opened, _) = play scene [Input.Up, Input.Enter]
            (advanced, _) = Engine.step opened Input.Enter
            (finished, _) = Engine.step advanced Input.Enter
            (cancelled, _) = Engine.step advanced Input.Escape
         in assertBool "Expected completion to set permission and cancellation to leave it unset" $
              interactionText opened == Just "Hello."
                && interactionText advanced == Just "You may take a gift."
                && Map.lookup permission (flags advanced) == Nothing
                && Map.lookup permission (flags cancelled) == Nothing
                && Map.lookup permission (flags finished) == Just True
                && activeInteraction finished == Nothing,
      testCase "Navigate choices with arrows and j/k, clamping at the ends" $
        let (opened, _) = Engine.step permitted Input.Enter
         in forM_ [(Input.Down, Input.Up), (Input.Character 'j', Input.Character 'k')] $ \(next, previous) -> do
              let (selected, _) = Engine.step opened next
                  (lastChoice, extra) = Engine.step selected next
                  (firstChoice, _) = Engine.step selected previous
              assertBool "Expected selection commands, bounds, and a return to the first choice" $
                Engine.commandFor opened next == Just Command.SelectNext
                  && Engine.commandFor opened previous == Just Command.SelectPrevious
                  && (activeSelectedChoice =<< activeInteraction selected) == Just 1
                  && lastChoice == selected
                  && null extra
                  && (activeSelectedChoice =<< activeInteraction firstChoice) == Just 0,
      testCase "No and Escape both leave the offer available" $
        forM_ [[Input.Enter, Input.Down, Input.Enter], [Input.Enter, Input.Escape]] $ \inputs ->
          let (closed, _) = play permitted inputs
           in assertBool "Expected no lasting changes after declining or cancelling" (closed == permitted),
      testCase "Block exploration and party commands during an interaction" $
        let (opened, _) = Engine.step permitted Input.Enter
         in forM_ [Input.Left, Input.Right, Input.Character 'h', Input.Character 'l', Input.Character 'p'] $ \input ->
              assertBool "Expected the active choice and world to remain unchanged" $
                Engine.step opened input == (opened, []) && Engine.eventsFor opened (Command.Walk East) == [],
      testCase "A full party shows a message and consumes nothing" $
        let starting = permitted{maxPartySize = 1, party = Party [critter]}
            (rejected, _) = play starting [Input.Enter, Input.Enter]
         in assertBool "Expected a visible rejection with the gift, object and flags intact" $
              interactionText rejected == Just "Your party is full."
                && party rejected == party starting
                && objects rejected == objects starting
                && flags rejected == flags starting,
      testCase "Effects-only interactions can complete without a dialogue view" $
        let script = Interaction [] [] [] [SetFlag claimed True]
            starting = scene{objects = Map.adjust (\object -> object{objectInteractions = [script]}) capsuleId (objects scene)}
            (finished, events) = Engine.step starting Input.Enter
         in assertBool "Expected the effect fact with no pending interaction" $
              events == [Event.FlagChanged claimed True] && activeInteraction finished == Nothing,
      testCase "A selected interaction remains a snapshot when the source changes" $
        let (opened, _) = Engine.step permitted Input.Enter
            changed = opened{objects = Map.adjust (\object -> object{objectInteractions = []}) capsuleId (objects opened)}
            (finished, _) = Engine.step changed Input.Enter
         in assertBool "Expected the originally selected gift effects" $
              members (party finished) == [critter] && not (Map.member capsuleId (objects finished))
    ]

objectTests :: TestTree
objectTests =
  testGroup
    "World objects"
    [ testCase "Block walking onto an object while allowing interaction"
        $ assertBool "Expected an occupied cell and a valid interaction in front"
        $ Engine.step scene Input.Right == (scene, []) && not (null (snd (Engine.step scene Input.Enter))),
      testCase "Ignore objects on other maps" $
        let remote = scene{objects = Map.map (\object -> object{objectLocation = Location (MapId "remote") (Position 2 1)}) (objects scene)}
         in assertBool "Expected another map's object to neither block nor receive interaction" $
              objectAt remote (Position 2 1) == Nothing
                && Engine.step remote Input.Right == (remote{playerPosition = Position 2 1}, [Event.PlayerMoved (Position 2 1)])
                && Engine.step remote Input.Enter == (remote, []),
      testCase "Block ledge landings and warp arrivals occupied by objects" $ do
        let layout = World [replicate 4 (Just Ground), [Just Ground, Just Ground, Just (Ledge South), Just Ground], replicate 4 (Just Ground), replicate 4 (Just Ground)] []
            ledge = scene{maps = Map.singleton (MapId "test") layout, playerDirection = South, playerPosition = Position 2 3}
            entrance = World (replicate 5 (replicate 5 (Just Ground))) [Warp (Location (MapId "test") (Position 2 1)) Nothing (Position 2 2)]
            warp = scene{maps = Map.singleton (MapId "test") entrance, playerDirection = North, playerPosition = Position 2 1}
        assertBool "Expected no jump onto the object" (Engine.step ledge Input.Down == (ledge, []))
        assertBool "Expected no warp onto the object" (Engine.step warp Input.Up == (warp, []))
    ]

permission :: FlagId
permission = FlagId "guide.permission"

permitted :: GameState
permitted = scene{flags = Map.singleton permission True}

play :: GameState -> [Input.Input] -> (GameState, [Event.Event])
play starting = foldl' advance (starting, [])
  where
    advance (state, history) input = let (next, events) = Engine.step state input in (next, history <> events)

rows :: GameState -> [Text]
rows = map (Text.concat . map Cell.glyph) . TUI.renderFrame

saveTests :: TestTree
saveTests =
  testGroup
    "Persistence"
    [ testCase "Save and resume a pending choice, then replay the completed gift" $
        withSaveFile $ \path -> do
          let (opened, history) = Engine.step permitted Input.Enter
          written <- Save.save path history
          loaded <- Save.load path
          assertBool "Expected the pending choice snapshot to survive disk" $
            written == Right () && fmap (Save.apply permitted) loaded == Right opened
          let (finished, confirmed) = Engine.step opened Input.Enter
          _ <- Save.save path (history <> confirmed)
          resumed <- Save.load path
          assertBool "Expected receipt, removal and flag changes to replay exactly" $
            fmap (Save.apply permitted) resumed == Right finished
              && members (party finished) == [critter]
              && Map.lookup claimed (flags finished) == Just True,
      testCase "Replay does not re-evaluate interaction conditions" $
        let (finished, history) = play permitted [Input.Enter, Input.Enter]
            different = permitted{flags = Map.empty}
            replayed = Save.apply different history
         in assertBool "Expected the recorded gift even with different starting conditions" $
              party replayed == party finished
                && objects replayed == objects finished
                && Map.lookup claimed (flags replayed) == Just True
    ]

scene :: GameState
scene =
  initialState
    { maps = Map.singleton (MapId "test") (World (replicate 5 (replicate 5 (Just Ground))) []),
      maxPartySize = 2,
      npcs = Map.singleton (NpcId "guide") (Npc [] [Interaction [] [] ["Hello.", "You may take a gift."] [SetFlag permission True]] (Location (MapId "test") (Position 1 2))),
      objects =
        Map.singleton
          capsuleId
          ( Object
              "🔴"
              [ Interaction [] [Flag claimed True] ["Already claimed."] [],
                Interaction [] [Flag permission False] ["Ask the guide first."] [],
                gift
              ]
              (Location (MapId "test") (Position 2 1))
          ),
      playerDirection = East,
      playerMap = MapId "test",
      playerPosition = Position 1 1
    }

tests :: TestTree
tests = testGroup "Interactions" [codecTests, effectTests, exampleTests, interactionTests, objectTests, saveTests, tuiTests]

tuiTests :: TestTree
tuiTests =
  testGroup
    "Rendering"
    [ testCase "Show objects and replace the removed object with its terrain" $
        let (finished, _) = play permitted [Input.Enter, Input.Enter]
         in assertBool "Expected the object before pickup and ground after pickup" $
              Text.isInfixOf "🔴" (TUI.render permitted) && not (Text.isInfixOf "🔴" (TUI.render finished)),
      testCase "Show the question, selected choice, controls and selection changes" $
        let (opened, _) = Engine.step permitted Input.Enter
            (selected, _) = Engine.step opened Input.Down
            frame = TUI.renderFrame opened
         in assertBool "Expected a fixed viewport and visible, changing choice marker" $
              length frame == TUI.height
                && all ((== TUI.width * 2) . sum . map Cell.columns) frame
                && map Text.stripEnd (take 4 (rows opened)) == ["Take this critter?", "", "> Yes", "  No"]
                && map Text.stripEnd (take 4 (rows selected)) == ["Take this critter?", "", "  Yes", "> No"]
                && Text.isInfixOf "Enter: Confirm" (TUI.render opened),
      testCase "Show scripted dialogue over the world and restore it on cancellation" $
        let (opened, _) = Engine.step scene Input.Enter
            (closed, _) = Engine.step opened Input.Escape
         in assertBool "Expected the denial message and the original frame after closing" $
              Text.isInfixOf "Ask the guide first." (TUI.render opened)
                && TUI.renderFrame closed == TUI.renderFrame scene
    ]

withSaveFile :: (FilePath -> IO a) -> IO a
withSaveFile action = bracket create removeFile action
  where
    create = do
      directory <- getTemporaryDirectory
      (path, handle) <- openTempFile directory "critters-interaction-test"
      hClose handle
      pure path
