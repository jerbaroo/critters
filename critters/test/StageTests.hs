module StageTests (tests) where

import BattleTests (fixture, start)
import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (forM_)
import Critters.Battle.Common (Context (..), Side (..))
import Critters.Battle.Common qualified as Common
import Critters.Battle.Opponent qualified as Opponent
import Critters.Expression (Expression (..))
import Critters.Stage qualified as Stage
import Critters.State.Battle (Battle (..))
import Critters.State.Battle qualified as Battle
import Critters.State.CombatConfig qualified as Config
import Critters.State.GameState qualified as GameState
import Critters.State.Move (defaultMove)
import Critters.State.Move qualified as Move
import Critters.State.MoveEffect (MoveEffect (..), Target (..))
import Critters.State.Rules qualified as Rules
import Critters.State.Stage (StageDefinition (..), StageDefinitions (..), StageValues (..), stageDefinition)
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
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
        "Stages"
        [ testCase "Decode explicit stage definitions keyed by stat name" $ do
            Aeson.eitherDecode @StageDefinitions "{\"defense\":{\"limit\":2,\"multiplier\":\"1 + stage / 4\"}}"
                @?= Right (StageDefinitions (Map.singleton "defense" defense))
            Aeson.eitherDecode (Aeson.encode definitions) @?= Right definitions
            Aeson.eitherDecode (Aeson.encode (StageValues (Map.singleton "defense" (-1)))) @?= Right (StageValues (Map.singleton "defense" (-1)))
        , testCase "Require limits and multipliers and reject invalid definitions" $
            forM_ ["{}", "{\"limit\":2}", "{\"multiplier\":\"1\"}", "{\"limit\":-1,\"multiplier\":\"1\"}", "{\"limit\":101,\"multiplier\":\"1\"}", "{\"limit\":2,\"multiplier\":\"hp\"}", "{\"limit\":2,\"multiplier\":\"stage +\"}"] $ \source ->
                assertBool "Expected an invalid definition to fail decoding" (isLeft (Aeson.eitherDecode @StageDefinition source))
        , testCase "Start defined stages at zero and apply their own multiplier" $ do
            Stage.stageFactor definitions empty "defense" @?= 1
            Stage.stageFactor definitions (fst (Stage.changeStage definitions empty "defense" (-1))) "defense" @?= 0.75
            Stage.stageFactor definitions (fst (Stage.changeStage definitions empty "defense" 2)) "defense" @?= 1.5
        , testCase "Use authored formulas even for accuracy and evasion" $ do
            let authored = StageDefinitions (Map.fromList [("accuracy", StageDefinition 1 (Expression "3 + stage")), ("evasion", StageDefinition 3 (Expression "2 + stage / 2"))])
            Stage.stageFactor authored empty "accuracy" @?= 3
            Stage.stageFactor authored (StageValues (Map.singleton "evasion" 2)) "evasion" @?= 3
        , testCase "Preserve the migrated default multipliers" $
            forM_ [("defense", -2, 0.5), ("defense", -1, 2 / 3), ("defense", 0, 1), ("defense", 1, 1.5), ("defense", 2, 2), ("accuracy", -1, 0.75), ("accuracy", 1, 4 / 3), ("evasion", 2, 5 / 3)] $ \(name, value, expected) ->
                Stage.stageFactor fixture.combat.stages (StageValues (Map.singleton name value)) name @?= expected
        , testCase "Clamp changes independently and preserve other stage values" $ do
            let initial = StageValues (Map.fromList [("defense", 1), ("focus", -1)])
            fst (Stage.changeStage definitions initial "defense" maxBound) @?= StageValues (Map.fromList [("defense", 2), ("focus", -1)])
            fst (Stage.changeStage definitions initial "defense" minBound) @?= StageValues (Map.fromList [("defense", -1), ("focus", -1)])
            fst (Stage.changeStage definitions empty "focus" maxBound) @?= StageValues (Map.singleton "focus" 1)
            fst (Stage.changeStage definitions empty "defense" minBound) @?= StageValues (Map.singleton "defense" (-2))
            fst (Stage.changeStage definitions (StageValues (Map.singleton "defense" 2)) "defense" 1) @?= StageValues (Map.singleton "defense" 2)
        , testCase "Support a zero limit without changing the stage" $ do
            let locked = StageDefinitions (Map.singleton "defense" (StageDefinition 0 defense.multiplier))
            fst (Stage.changeStage locked empty "defense" 2) @?= StageValues (Map.singleton "defense" 0)
        , testCase "Raise named errors for missing definitions without fallbacks" $ do
            assertRuntimeError "Missing stage definition: missing" (evaluate (stageDefinition definitions "missing"))
            assertRuntimeError "Missing stage definition: missing" (evaluate (Stage.stageFactor definitions empty "missing"))
            assertRuntimeError "Missing stage definition: missing" (evaluate (Map.lookup "missing" (fst (Stage.changeStage definitions empty "missing" 0)).values))
        , testCase "Raise expression failures from stage multipliers" $
            assertRuntimeError "Division by zero" (evaluate (Stage.stageFactor (StageDefinitions (Map.singleton "defense" defense{multiplier = Expression "1 / 0"})) empty "defense"))
        , testCase "Apply the same stage limits in battle state and opponent scoring" $ do
            let context = battleContext{battle = battleContext.battle{Battle.config = battleContext.battle.config{Config.stages = StageDefinitions (Map.union definitions.definitions battleContext.battle.config.stages.definitions), Config.rules = battleContext.battle.config.rules{Rules.aiScoreFormula = Expression "status"}}, Battle.enemyStages = StageValues (Map.singleton "defense" 1)}}
                move = defaultMove{Move.power = 0, Move.effects = [ChangeStage Self "defense" 8]}
                (_, next) = Common.setStage Enemy "defense" 8 context
            Opponent.scoreMove context move @?= 1
            assertBool "Expected only the targeted stage values to change" (next == context{battle = context.battle{Battle.enemyStages = StageValues (Map.singleton "defense" 2)}})
        , testCase "Validate persisted stage values against their own limit" $ do
            let valid = battleContext.battle{Battle.config = battleContext.battle.config{Config.stages = definitions}, Battle.playerStages = StageValues (Map.singleton "defense" 2)}
            assertBool "Expected valid saved stage values to round trip" (Aeson.eitherDecode (Aeson.encode valid) == Right valid)
            assertBool "Expected a saved value beyond its own limit to fail" (isLeft (Aeson.eitherDecode @Battle (Aeson.encode valid{Battle.playerStages = StageValues (Map.singleton "defense" 3)})))
        , testCase "Raise a named error for an undefined persisted stage" $
            assertRuntimeError "Missing stage definition: missing" (evaluate (Aeson.eitherDecode @Battle (Aeson.encode battleContext.battle{Battle.playerStages = StageValues (Map.singleton "missing" 0)})))
        ]
  where
    battleContext = maybe (error "Expected an active battle in the fixture.") id (Common.contextFor (start fixture))
    defense = StageDefinition 2 (Expression "1 + stage / 4")
    definitions = StageDefinitions (Map.fromList [("defense", defense), ("focus", StageDefinition 1 (Expression "max(0, 1 + stage)"))])
    empty = StageValues Map.empty
