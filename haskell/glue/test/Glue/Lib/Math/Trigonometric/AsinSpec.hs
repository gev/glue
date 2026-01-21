module Glue.Lib.Math.Trigonometric.AsinSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Trigonometric.Asin qualified as Asin
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Trigonometric.Asin (Test asin function)" do
    describe "Arcsine function" do
        it "returns 0 for asin(0)" do
            let args = [Integer 0]
            result <- runEvalSimple (apply Asin.asin args) []
            case result of
                Left err -> expectationFailure $ "Asin failed: " <> show err
                Right (res, _) -> res `shouldBe` Float 0

        it "returns π/2 for asin(1)" do
            let args = [Integer 1]
            result <- runEvalSimple (apply Asin.asin args) []
            case result of
                Left err -> expectationFailure $ "Asin failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - pi / 2) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (apply Asin.asin args) []
            result `shouldSatisfy` isLeft
