module Glue.Lib.Math.Power.SqrtSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Power.Sqrt qualified as Sqrt
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Power.Sqrt (Test sqrt function)" do
    describe "Square root function" do
        it "returns 2 for sqrt(4)" do
            let args = [Integer 4]
            result <- runEvalLegacy (Sqrt.sqrt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sqrt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 2

        it "returns 3 for sqrt(9)" do
            let args = [Integer 9]
            result <- runEvalLegacy (Sqrt.sqrt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sqrt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 3

        it "returns 0 for sqrt(0)" do
            let args = [Integer 0]
            result <- runEvalLegacy (Sqrt.sqrt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sqrt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 0

        it "fails with negative numbers" do
            let args = [Float (-4)]
            result <- runEvalLegacy (Sqrt.sqrt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalLegacy (Sqrt.sqrt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalLegacy (Sqrt.sqrt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Sqrt.sqrt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
