module Glue.Lib.Math.Power.ExpSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (toRealFloat)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Power.Exp qualified as Exp
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Power.Exp (Test exp function)" do
    describe "Exponential function" do
        it "returns e^0 = 1" do
            let args = [Integer 0]
            result <- runEvalLegacy (Exp.exp args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Exp failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 1

        it "returns e^1 = e" do
            let args = [Integer 1]
            result <- runEvalLegacy (Exp.exp args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Exp failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - exp 1) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns e^2" do
            let args = [Integer 2]
            result <- runEvalLegacy (Exp.exp args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Exp failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - exp 2) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalLegacy (Exp.exp args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalLegacy (Exp.exp args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Exp.exp args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
