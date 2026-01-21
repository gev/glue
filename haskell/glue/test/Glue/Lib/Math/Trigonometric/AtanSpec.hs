module Glue.Lib.Math.Trigonometric.AtanSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Trigonometric.Atan qualified as Atan
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Trigonometric.Atan (Test atan function)" do
    describe "Arctangent function" do
        it "returns 0 for atan(0)" do
            let args = [Integer 0]
            result <- runEvalSimple (apply Atan.atan args) []
            case result of
                Left err -> expectationFailure $ "Atan failed: " <> show err
                Right (res, _) -> res `shouldBe` Float 0

        it "returns π/4 for atan(1)" do
            let args = [Integer 1]
            result <- runEvalSimple (apply Atan.atan args) []
            case result of
                Left err -> expectationFailure $ "Atan failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - pi / 4) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (apply Atan.atan args) []
            result `shouldSatisfy` isLeft
