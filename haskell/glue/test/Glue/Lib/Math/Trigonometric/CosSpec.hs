module Glue.Lib.Math.Trigonometric.CosSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Trigonometric.Cos qualified as Cos
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Trigonometric.Cos (Test cos function)" do
    describe "Cosine function" do
        it "returns 1 for cos(0)" do
            let args = [Integer 0]
            result <- runEvalSimple (apply Cos.cos args) []
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _) -> res `shouldBe` Float 1

        it "returns 0 for cos(pi/2)" do
            let args = [Float (pi / 2)]
            result <- runEvalSimple (apply Cos.cos args) []
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns -1 for cos(pi)" do
            let args = [Float pi]
            result <- runEvalSimple (apply Cos.cos args) []
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - (-1)) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (apply Cos.cos args) []
            result `shouldSatisfy` isLeft
