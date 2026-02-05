module Glue.Lib.Math.Trigonometric.TanSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Trigonometric.Tan qualified as Tan
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Trigonometric.Tan (Test tan function)" do
    describe "Tangent function" do
        it "returns 0 for tan(0)" do
            let args = [Integer 0]
            result <- runEvalSimple (apply Tan.tan args) []
            case result of
                Left err -> expectationFailure $ "Tan failed: " <> show err
                Right (res, _) -> res `shouldBe` Float 0

        it "returns 1 for tan(pi/4)" do
            let args = [Float (pi / 4)]
            result <- runEvalSimple (apply Tan.tan args) []
            case result of
                Left err -> expectationFailure $ "Tan failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 1) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (apply Tan.tan args) []
            result `shouldSatisfy` isLeft
