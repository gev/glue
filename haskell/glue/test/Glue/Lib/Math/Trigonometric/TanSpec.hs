module Glue.Lib.Math.Trigonometric.TanSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Trigonometric.Tan qualified as Tan (tan)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Trigonometric.Tan (Test tan function)" do
    describe "Tangent function" do
        it "returns 0 for tan(0)" do
            let args = [Integer 0]
            result <- runEvalSimple (Tan.tan args) []
            case result of
                Left err -> expectationFailure $ "Tan failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 0

        it "returns 1 for tan(π/4)" do
            let args = [Float (pi / 4)]
            result <- runEvalSimple (Tan.tan args) []
            case result of
                Left err -> expectationFailure $ "Tan failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 1) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (Tan.tan args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (Tan.tan args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Tan.tan args) []
            result `shouldSatisfy` isLeft
