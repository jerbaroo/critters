module SceneTests (tests) where

import BattleTests (fixture, start)
import Control.Monad (forM_)
import Critters.Battle.Action (Action (..))
import Critters.Battle.Turn qualified as Turn
import Critters.Command qualified as Command
import Critters.Engine qualified as Engine
import Critters.Event (Event (..))
import Critters.Input qualified as Input
import Critters.Save qualified as Save
import Critters.State.Battle (Battle (..))
import Critters.State.BattleDefinition (BattleDefinition (..))
import Critters.State.BattlePhase (BattlePhase (..))
import Critters.State.CombatConfig (CombatConfig (..))
import Critters.State.Effect (Effect (..))
import Critters.State.FlagId (FlagId (..))
import Critters.State.GameState (GameState (..))
import Critters.State.Location (Location (..))
import Critters.State.MapId (MapId (..))
import Critters.State.MoveId (MoveId (..))
import Critters.State.Npc (Npc (..))
import Critters.State.NpcId (NpcId (..))
import Critters.State.Object (Object (..))
import Critters.State.ObjectId (ObjectId (..))
import Critters.State.Position (Position (..))
import Critters.State.Tile (Tile (..))
import Critters.State.World (World (..))
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

scene :: GameState
scene = fixture{maps = Map.singleton (MapId "overworld") (World (replicate 4 (replicate 5 (Just Ground))) []), npcs = Map.singleton (NpcId "rival") (Npc [] [] (Location (MapId "overworld") (Position 0 1))), playerPosition = Position 4 1, combat = (combat fixture){configBattles = Map.adjust (\definition -> definition{definitionIntroduction = ["Meet my new partner!", "Let's battle!"]}) "practice" (configBattles (combat fixture))}}

tests :: TestTree
tests =
  testGroup
    "Scripted battle introduction"
    [ testCase "Expand the authored path into ordered movement facts before battle entry" $
        let identifier = NpcId "rival"
            path = [Position 1 1, Position 2 1, Position 3 1]
            effects = [WalkNpc identifier path, StartBattle "practice"]
         in case Engine.eventsForEffects scene effects of
              Left _ -> assertBool "Expected a valid scripted path" False
              Right events -> do
                let moved = [NpcMoved identifier (Location (MapId "overworld") position) | position <- path]
                    replayed = Save.apply scene events
                assertBool "Expected every authored tile followed by the battle entry" $
                  take 3 events == moved
                    && length events == 4
                    && fmap location (Map.lookup identifier (npcs replayed)) == Just (Location (MapId "overworld") (Position 3 1))
                    && fmap battlePhase (battle replayed) == Just Introduction
                    && playerPosition replayed == playerPosition scene
                    && party replayed == party scene
                    && Aeson.eitherDecode (Aeson.encode events) == Right events,
      testCase "Reject invalid paths atomically without keeping earlier effects or steps"
        $ forM_
          [ (scene, NpcId "missing", [Position 1 1]),
            (scene, NpcId "rival", [Position 1 1, Position 3 1]),
            (scene, NpcId "rival", [Position 1 1, Position 2 1, Position 3 1, Position 4 1]),
            (scene{npcs = Map.insert (NpcId "aide") (Npc [] [] (Location (MapId "overworld") (Position 2 1))) (npcs scene)}, NpcId "rival", [Position 1 1, Position 2 1]),
            (scene{objects = Map.singleton (ObjectId "desk") (Object "X" [] (Location (MapId "overworld") (Position 2 1)))}, NpcId "rival", [Position 1 1, Position 2 1]),
            (scene{maps = Map.singleton (MapId "overworld") (World (replicate 4 [Just Ground, Just Ground, Just Wall]) [])}, NpcId "rival", [Position 1 1, Position 2 1]),
            (scene, NpcId "rival", [Position 0 2, Position 0 3, Position 0 4])
          ]
        $ \(state, identifier, path) ->
          assertBool "Expected the entire effect batch to fail" $
            isLeft (Engine.eventsForEffects state [SetFlag (FlagId "chosen") True, WalkNpc identifier path, StartBattle "practice"]),
      testCase "Only Enter advances the introduction, then sends combatants out without taking a turn" $ do
        let opened = start scene
            (next, advanced) = Engine.step opened Input.Enter
            (ready, sentOut) = Engine.step next Input.Enter
        assertBool "Expected a battle-owned dialogue index and phase, with replayable transitions" $
          fmap battlePhase (battle opened) == Just Introduction
            && advanced == [BattleIntroductionAdvanced 1]
            && next == opened{battle = fmap (\active -> active{battleIntroductionIndex = 1}) (battle opened)}
            && sentOut == [BattleCombatantsSentOut]
            && ready == next{battle = fmap (\active -> active{battlePhase = ActionMenu}) (battle next)}
            && Save.apply opened (advanced <> sentOut) == ready
            && fmap battleTurn (battle ready) == Just 0
            && party ready == party opened
            && randomSeed ready == randomSeed opened
        forM_ [Input.Escape, Input.Up, Input.Down, Input.Left, Input.Right] $ \input ->
          assertBool "Expected no skipping or battle menu navigation during introduction" (Engine.step opened input == (opened, []))
        assertBool "Expected combat actions to be unavailable before the dialogue ends" $
          null (Engine.eventsFor opened (Command.ActInBattle (UseMove (MoveId "strike"))))
            && null (Turn.resolveAction opened (UseMove (MoveId "strike"))),
      testCase "Battles without dialogue still open the action menu immediately"
        $ assertBool "Expected the existing battle flow when introduction is empty"
        $ fmap battlePhase (battle (start fixture)) == Just ActionMenu,
      testCase "Decode authored paths and round trip the new event facts" $ do
        assertBool "Expected the JSON walking path to retain its exact order" $
          Aeson.eitherDecode @Effect "{\"type\":\"walk-npc\",\"npc\":\"rival\",\"path\":[{\"x\":1,\"y\":1},{\"x\":2,\"y\":1}]}" == Right (WalkNpc (NpcId "rival") [Position 1 1, Position 2 1])
        forM_ [NpcMoved (NpcId "rival") (Location (MapId "overworld") (Position 1 1)), BattleIntroductionAdvanced 1, BattleCombatantsSentOut] $ \event ->
          assertBool "Expected events to survive persistence" (Aeson.eitherDecode (Aeson.encode event) == Right event)
    ]
