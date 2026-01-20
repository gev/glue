module Glue.Lib.Math.Power.ExpSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Power.Exp qualified as Exp
import Glue.TestUtils ()
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Power.Exp (Test exp function)" do
    describe "Exponential function" do
        it "returns e^0 = 1" do
            let args = [Integer 0]
            result <- runEvalSimple (apply Exp.exp args) []
            case result of
                Left err -> expectationFailure $ "Exp failed: " <> show err
                Right (res, _) -> res `shouldBe` Float 1

        it "returns e^1 = e" do
            let args = [Integer 1]
            result <- runEvalSimple (apply Exp.exp args) []
            case result of
                Left err -> expectationFailure $ "Exp failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - Prelude.exp 1) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns e^2" do
            let args = [Integer 2]
            result <- runEvalSimple (apply Exp.exp args) []
            case result of
                Left err -> expectationFailure $ "Exp failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - Prelude.exp 2) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (apply Exp.exp args) []
            result `shouldSatisfy` isLeft
