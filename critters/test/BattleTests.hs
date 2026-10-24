module BattleTests (tests) where

import Critters.Formula (Formula (..))
import Critters.Formula qualified as Formula
import Critters.State.Move (Move (..), defaultMove)
import Critters.State.MoveEffect (MoveEffect (..), Target (..))
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests = testGroup "Battle"
  [ testGroup "Rules"
      [ testCase "Evaluate arithmetic, rounding and conditional rules" $ do
          assertBool "Expected game-defined damage" (Formula.evaluate (Map.fromList [("power", 30), ("attack", 15), ("defense", 8)]) (Formula "max(1, floor(power * attack / defense / 5))") == Right 11)
          assertBool "Expected lazy conditionals and comparisons" (Formula.evaluate Map.empty (Formula "if(2 >= 1, 2^3 + ceil(1.2), 1/0)") == Right 10)
          assertBool "Expected a finite rule result" (isLeft (Formula.evaluate Map.empty (Formula "1/0")))
          assertBool "Expected unknown variables to fail" (isLeft (Formula.evaluate Map.empty (Formula "missing + 1"))),
        testCase "Reject malformed expressions and invalid function arity" $ do
          assertBool "Expected malformed expressions to fail decoding" (isLeft (Aeson.eitherDecode @Formula "\"floor(\""))
          assertBool "Expected arity validation" (isLeft (Aeson.eitherDecode @Formula "\"min(1)\""))
          assertBool "Expected variable discovery" (Formula.variables (Formula "max(attack, attack / defense)") == Right ["attack", "defense"])
      ],
    testGroup "Moves"
      [ testCase "Decode an independently written move and defaults" $
          assertBool "Expected configured power and default targeting rules" (Aeson.eitherDecode @Move "{\"name\":\"Basic strike\",\"power\":20}" == Right defaultMove),
        testCase "Round trip conditional move effects" $
          let move = defaultMove{moveEffects = [Chance 0.25 (InflictStatus Opponent "slow"), ChangeHealth Self (Formula "damage / 2"), ChangeStage Opponent "defense" (-1)]}
           in assertBool "Expected all authored effects to survive serialization" (Aeson.eitherDecode (Aeson.encode move) == Right move),
        testCase "Reject invalid probabilities, resources and hit counts" $
          mapM_ (\encoded -> assertBool "Expected invalid move data to fail" (isLeft (Aeson.eitherDecode @Move encoded)))
            ["{\"name\":\"x\",\"accuracy\":2}", "{\"name\":\"x\",\"maxUses\":-1}", "{\"name\":\"x\",\"minHits\":2,\"maxHits\":1}", "{\"name\":\"x\",\"effects\":[{\"type\":\"chance\",\"probability\":-1,\"effect\":{\"type\":\"clear-status\"}}]}"]
      ]
  ]
