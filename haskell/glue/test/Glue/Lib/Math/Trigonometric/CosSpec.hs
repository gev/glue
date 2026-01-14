module Glue.Lib.Math.Trigonometric.CosSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Trigonometric.Cos qualified as Cos (cos)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Trigonometric.Cos (Test cos function)" do
    describe "Cosine function" do
        it "returns 1 for cos(0)" do
            let args = [Integer 0]
            result <- runEvalSimple (Cos.cos args) []
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _) -> res `shouldBe` Float 1

        it "returns 0 for cos(π/2)" do
            let args = [Float (pi / 2)]
            result <- runEvalSimple (Cos.cos args) []
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns -1 for cos(π)" do
            let args = [Float pi]
            result <- runEvalSimple (Cos.cos args) []
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - (-1)) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (Cos.cos args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (Cos.cos args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Cos.cos args) []
            result `shouldSatisfy` isLeft
