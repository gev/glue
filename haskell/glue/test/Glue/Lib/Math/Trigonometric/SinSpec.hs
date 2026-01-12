module Glue.Lib.Math.Trigonometric.SinSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Trigonometric.Sin qualified as Sin (sin)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Trigonometric.Sin (Test sin function)" do
    describe "Sine function" do
        it "returns 0 for sin(0)" do
            let args = [Integer 0]
            result <- runEvalSimple (Sin.sin args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sin failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 0

        it "returns 1 for sin(π/2)" do
            let args = [Float (pi / 2)]
            result <- runEvalSimple (Sin.sin args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sin failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 1) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns 0 for sin(π)" do
            let args = [Float pi]
            result <- runEvalSimple (Sin.sin args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sin failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (Sin.sin args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (Sin.sin args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Sin.sin args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

-- QuickCheck properties removed for now - need proper IO testing setup
