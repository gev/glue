module Glue.Lib.Math.Power.SqrtSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Power.Sqrt qualified as Sqrt
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Power.Sqrt (Test sqrt function)" do
    describe "Square root function" do
        it "returns 2 for sqrt(4)" do
            let args = [Integer 4]
            result <- runEvalSimple (Sqrt.sqrt args) []
            case result of
                Left err -> expectationFailure $ "Sqrt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 2

        it "returns 3 for sqrt(9)" do
            let args = [Integer 9]
            result <- runEvalSimple (Sqrt.sqrt args) []
            case result of
                Left err -> expectationFailure $ "Sqrt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 3

        it "returns 0 for sqrt(0)" do
            let args = [Integer 0]
            result <- runEvalSimple (Sqrt.sqrt args) []
            case result of
                Left err -> expectationFailure $ "Sqrt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 0

        it "returns NaN for negative numbers" do
            let args = [Float (-4)]
            result <- runEvalSimple (Sqrt.sqrt args) []
            case result of
                Left err -> expectationFailure $ "Sqrt failed: " <> show err
                Right (res, _, _) -> case res of
                    Float f | isNaN f -> f `shouldSatisfy` isNaN
                    _ -> expectationFailure "Expected NaN"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (Sqrt.sqrt args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (Sqrt.sqrt args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Sqrt.sqrt args) []
            result `shouldSatisfy` isLeft
