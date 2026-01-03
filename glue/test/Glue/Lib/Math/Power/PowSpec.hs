module Glue.Lib.Math.Power.PowSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Power.Pow qualified as Pow
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Power.Pow (Test pow function)" do
    describe "Power function" do
        it "returns 2^3 = 8" do
            let args = [Number 2, Number 3]
            result <- runEvalLegacy (Pow.pow args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Pow failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 8

        it "returns 3^2 = 9" do
            let args = [Number 3, Number 2]
            result <- runEvalLegacy (Pow.pow args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Pow failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 9

        it "returns 2^0 = 1" do
            let args = [Number 2, Number 0]
            result <- runEvalLegacy (Pow.pow args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Pow failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 1

        it "returns 0^5 = 0" do
            let args = [Number 0, Number 5]
            result <- runEvalLegacy (Pow.pow args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Pow failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 0

        it "fails with non-numbers (first arg)" do
            let args = [String "hello", Number 2]
            result <- runEvalLegacy (Pow.pow args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers (second arg)" do
            let args = [Number 2, String "hello"]
            result <- runEvalLegacy (Pow.pow args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (one)" do
            let args = [Number 2]
            result <- runEvalLegacy (Pow.pow args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (three)" do
            let args = [Number 2, Number 3, Number 4]
            result <- runEvalLegacy (Pow.pow args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Pow.pow args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
