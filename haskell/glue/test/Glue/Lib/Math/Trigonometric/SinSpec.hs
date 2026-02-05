module Glue.Lib.Math.Trigonometric.SinSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Trigonometric.Sin qualified as Sin
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Trigonometric.Sin (Test sin function)" do
    describe "Sine function" do
        it "returns 0 for sin(0)" do
            let args = [Integer 0]
            result <- runEvalSimple (apply Sin.sin args) []
            case result of
                Left err -> expectationFailure $ "Sin failed: " <> show err
                Right (res, _) -> res `shouldBe` Float 0

        it "returns 1 for sin(pi/2)" do
            let args = [Float (pi / 2)]
            result <- runEvalSimple (apply Sin.sin args) []
            case result of
                Left err -> expectationFailure $ "Sin failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 1) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns 0 for sin(pi)" do
            let args = [Float pi]
            result <- runEvalSimple (apply Sin.sin args) []
            case result of
                Left err -> expectationFailure $ "Sin failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (apply Sin.sin args) []
            result `shouldSatisfy` isLeft
