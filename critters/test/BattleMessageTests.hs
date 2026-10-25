module BattleMessageTests (tests) where

import BattleTests (fixture, start)
import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (forM_)
import Critters.Battle.Action (Action (UseMove))
import Critters.Battle.Common (Side (..))
import Critters.Battle.Message qualified as Message
import Critters.Battle.Turn qualified as Turn
import Critters.Engine qualified as Engine
import Critters.Event (Event)
import Critters.Expression (Value (..))
import Critters.Interface.TUI qualified as TUI
import Critters.Interface.TUI.Cell qualified as Cell
import Critters.Stage (StageChange (..), StageOutcome (..))
import Critters.Stage qualified as Stage
import Critters.State.Battle qualified as Battle
import Critters.State.CombatConfig qualified as Config
import Critters.State.GameState qualified as GameState
import Critters.State.Move (defaultMove)
import Critters.State.Move qualified as Move
import Critters.State.MoveEffect (MoveEffect (..), Target (..))
import Critters.State.MoveId (MoveId (..))
import Critters.State.Species qualified as Species
import Critters.State.Stage (StageValues (..))
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Witch (from)

assertRuntimeError :: Text -> IO a -> Assertion
assertRuntimeError expected action = do
    result <- try @ErrorCall action
    case result of
        Left failure -> assertBool "Expected the named runtime error" (Text.isInfixOf expected (from (show failure)))
        Right _ -> assertBool "Expected a runtime error" False

tests :: TestTree
tests =
    testGroup
        "Battle messages"
        [ testCase "Describe actual increases, decreases and blocked changes"
            $ forM_
                [ (StageChange (-1) 0 (-1) "defense", Fell 1, "Your Leafling's Defence fell by 1 stage (0 → -1).")
                , (StageChange 2 0 2 "defense", Rose 2, "Your Leafling's Defence rose by 2 stages (0 → +2).")
                , (StageChange 6 5 3 "defense", Rose 1, "Your Leafling's Defence rose by 1 stage (+5 → +6).")
                , (StageChange (-6) (-5) (-3) "defense", Fell 1, "Your Leafling's Defence fell by 1 stage (-5 → -6).")
                , (StageChange 6 6 1 "defense", UpperLimit, "Your Leafling's Defence cannot go any higher.")
                , (StageChange (-6) (-6) (-1) "defense", LowerLimit, "Your Leafling's Defence cannot go any lower.")
                , (StageChange 0 0 0 "defense", Unchanged, "Your Leafling's Defence stayed unchanged (0).")
                , (StageChange 6 6 0 "defense", Unchanged, "Your Leafling's Defence stayed unchanged (+6).")
                ]
            $ \(change, outcome, expected) -> do
                Stage.stageOutcome change @?= outcome
                Message.renderStageChange fixture.combat Player "leafling" change @?= expected
        , testCase "Use the affected side and authored species name" $ do
            let config = fixture.combat{Config.species = Map.adjust (\value -> value{Species.name = "Moss Pup"}) "leafling" fixture.combat.species}
            Message.renderStageChange config Enemy "leafling" (StageChange (-1) 0 (-1) "defense")
                @?= "Opposing Moss Pup's Defence fell by 1 stage (0 → -1)."
        , testCase "Let games customize labels, subject wording and numeric details" $ do
            let config = fixture.combat{Config.messages = Map.insert "stageRose" "{subject}: {stat} {before} -> {after}; requested {requested}; multiplier {beforeMultiplier} -> {afterMultiplier}" (Map.insert "stagePlayerSubject" "Ally {species}" fixture.combat.messages), Config.statNames = Map.insert "defense" "Guard" fixture.combat.statNames}
            Message.renderStageChange config Player "leafling" (StageChange 2 1 4 "defense")
                @?= "Ally Leafling: Guard 1 -> 2; requested 4; multiplier 1.5 -> 2"
        , testCase "Support arbitrary battle templates through the expression module" $
            Message.renderMessage fixture.combat{Config.messages = Map.singleton "notice" "{name}: {amount * 2}"} "notice" (Map.fromList [("amount", NumberValue 2), ("name", TextValue "Guard")]) @?= "Guard: 4"
        , testCase "Reject missing templates, labels and invalid template expressions" $ do
            assertRuntimeError "Missing battle message: stageRose" (evaluate (Text.length (Message.renderStageChange fixture.combat{Config.messages = Map.delete "stageRose" fixture.combat.messages} Player "leafling" rise)))
            assertRuntimeError "Missing stat display name: defense" (evaluate (Text.length (Message.renderStageChange fixture.combat{Config.statNames = Map.empty} Player "leafling" rise)))
            assertRuntimeError "Battle message stageRose: Unknown expression placeholder: missing" (evaluate (Text.length (Message.renderStageChange fixture.combat{Config.messages = Map.insert "stageRose" "{missing}" fixture.combat.messages} Player "leafling" rise)))
        , testCase "Return actual before and after facts from the engine" $ do
            let initial = StageValues (Map.singleton "defense" 5)
                (values, change) = Stage.changeStage fixture.combat.stages initial "defense" 3
            values @?= StageValues (Map.singleton "defense" 6)
            change @?= StageChange 6 5 3 "defense"
        , testCase "Emit informative messages for both move targets and preserve replay" $
            forM_ [(Self, "Your Leafling's Defence fell by 1 stage (0 → -1)."), (Opponent, "Opposing Stonekin's Defence fell by 1 stage (0 → -1).")] $ \(target, expected) -> do
                let configured = fixture{GameState.combat = fixture.combat{Config.moves = Map.singleton (MoveId "strike") defaultMove{Move.power = 0, Move.effects = [ChangeStage target "defense" (-1)]}}}
                    opened = start configured
                    events = Turn.resolveAction opened (UseMove (MoveId "strike"))
                    result = foldl' Engine.applyEvent opened events
                assertBool "Expected the informative message to reach the battle log" (maybe False (elem expected . (.messages)) result.battle)
                let dialogue frame = Text.strip (Text.unwords [Text.strip (Text.dropEnd 1 (Text.drop 1 (Text.concat (map (.glyph) row)))) | row <- take 2 (drop (TUI.height - 3) frame)])
                map dialogue (TUI.dialogueFrames 1 expected result result) @?= [expected]
                assertBool "Expected serialized events to reproduce the same state" (fmap (foldl' Engine.applyEvent opened) (Aeson.eitherDecode @[Event] (Aeson.encode events)) == Right result)
        ]
  where
    rise = StageChange 1 0 1 "defense"
