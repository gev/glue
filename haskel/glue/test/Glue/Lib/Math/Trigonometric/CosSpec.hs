module Glue.Lib.Math.Trigonometric.CosSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Trigonometric.Cos qualified as Cos (cos)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Trigonometric.Cos (Test cos function)" do
    describe "Cosine function" do
        it "returns 1 for cos(0)" do
            let args = [Number 0]
            result <- runEvalLegacy (Cos.cos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 1

        it "returns 0 for cos(π/2)" do
            let args = [Number (fromFloatDigits @Double (pi / 2))]
            result <- runEvalLegacy (Cos.cos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns -1 for cos(π)" do
            let args = [Number (fromFloatDigits @Double pi)]
            result <- runEvalLegacy (Cos.cos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - (-1)) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalLegacy (Cos.cos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEvalLegacy (Cos.cos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Cos.cos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
