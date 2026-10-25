module ExpressionTests (tests) where

import Control.Exception (ErrorCall, try)
import Control.Exception qualified as Exception
import Control.Monad (forM_)
import Critters.Battle.Common qualified as Common
import Critters.Expression (Expression (..), Value (..))
import Critters.Expression qualified as Expression
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Witch (from)

tests :: TestTree
tests =
    testGroup
        "Expressions"
        [ testCase "Evaluate caller-supplied text and numeric placeholders" $ do
            Expression.evaluate environment (Expression "subject") @?= Right (TextValue "Your Bulbasaur")
            Expression.evaluate environment (Expression "before - after") @?= Right (NumberValue 1)
        , testCase "Interpolate text and computed numbers in a message" $
            Expression.interpolate environment "{subject}'s {stat} fell by {before - after} stage ({before} → {after})."
                @?= Right "Your Bulbasaur's Defence fell by 1 stage (0 → -1)."
        , testCase "Choose text with lazy conditionals" $ do
            let values = Map.insert "singular" (TextValue "stage") environment
            Expression.interpolate values "1 {if(before - after == 1, singular, missing)}" @?= Right "1 stage"
            Expression.evaluate values (Expression "if(0, 1 / 0, singular)") @?= Right (TextValue "stage")
        , testCase "Render empty templates, plain text and fractional values" $ do
            Expression.interpolate Map.empty "" @?= Right ""
            Expression.interpolate Map.empty "No change." @?= Right "No change."
            Expression.interpolate Map.empty "{3 / 2}, {0}, {-2}" @?= Right "1.5, 0, -2"
        , testCase "Insert placeholder text literally" $
            Expression.interpolate (Map.singleton "subject" (TextValue "{unknown}")) "Hello {subject}." @?= Right "Hello {unknown}."
        , testCase "Reject missing placeholders and malformed interpolation" $
            forM_ ["{missing}", "{", "{}", "{before", "before}", "{before +}", "{{before}}", "{min(1)}", "{1 / 0}"] $ \source ->
                assertBool ("Expected an interpolation error for " <> from source) (isLeft (Expression.interpolate environment source))
        , testCase "Reject text used as a numeric operand or condition" $
            forM_ ["subject + 1", "min(subject, 1)", "if(subject, 1, 0)"] $ \source ->
                assertBool "Expected a type error" (isLeft (Expression.evaluate environment (Expression source)))
        , testCase "Reject non-finite constants, results and supplied numbers" $ do
            forM_ ["1e999", "sqrt(-1)", "10 ^ 999"] $ \source ->
                assertBool "Expected a finite expression value" (isLeft (Expression.evaluate Map.empty (Expression source)))
            assertBool "Expected non-finite placeholders to fail" (isLeft (Expression.evaluate (Map.singleton "value" (NumberValue (0 / 0))) (Expression "value")))
        , testCase "Serialize expression source and discover placeholders" $ do
            let expression = Expression "max(before, before - after)"
            Aeson.eitherDecode (Aeson.encode expression) @?= Right expression
            Aeson.eitherDecode @Expression "\"stage + 1\"" @?= Right (Expression "stage + 1")
            Expression.variables expression @?= Right ["before", "after"]
        , testCase "Raise runtime errors when numeric game rules fail" $
            forM_ [("missing + 1", "missing"), ("1 / 0", "Division by zero")] $ \(source, expected) -> do
                result <- try @ErrorCall (Exception.evaluate (Common.evaluate (Expression source) []))
                case result of
                    Left failure -> assertBool "Expected the expression failure in the runtime error" (Text.isInfixOf expected (from (show failure)))
                    Right _ -> assertBool "Expected a runtime error instead of a numeric fallback" False
        ]
  where
    environment = Map.fromList [("after", NumberValue (-1)), ("before", NumberValue 0), ("stat", TextValue "Defence"), ("subject", TextValue "Your Bulbasaur")]
