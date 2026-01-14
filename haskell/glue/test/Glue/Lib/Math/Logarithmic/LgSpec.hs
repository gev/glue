module Glue.Lib.Math.Logarithmic.LgSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Logarithmic.Lg qualified as Lg
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Logarithmic.Lg (Test lg function)" do
    describe "Common logarithm function (base 10)" do
        it "returns lg(1) = 0" do
            let args = [Integer 1]
            result <- runEvalSimple (Lg.lg args) []
            case result of
                Left err -> expectationFailure $ "Lg failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs x < 1e-10)
                    _ -> expectationFailure "Expected a Float"

        it "returns lg(10) = 1" do
            let args = [Integer 10]
            result <- runEvalSimple (Lg.lg args) []
            case result of
                Left err -> expectationFailure $ "Lg failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 1) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns lg(100) = 2" do
            let args = [Integer 100]
            result <- runEvalSimple (Lg.lg args) []
            case result of
                Left err -> expectationFailure $ "Lg failed: " <> show err
                Right (res, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 2) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns -Infinity for zero" do
            let args = [Integer 0]
            result <- runEvalSimple (Lg.lg args) []
            case result of
                Left err -> expectationFailure $ "Lg failed: " <> show err
                Right (res, _) -> case res of
                    Float f | f == (-1 / 0) -> f `shouldBe` (-1 / 0)
                    _ -> expectationFailure "Expected -Infinity"

        it "returns NaN for negative numbers" do
            let args = [Float (-1)]
            result <- runEvalSimple (Lg.lg args) []
            case result of
                Left err -> expectationFailure $ "Lg failed: " <> show err
                Right (res, _) -> case res of
                    Float f | isNaN f -> f `shouldSatisfy` isNaN
                    _ -> expectationFailure "Expected NaN"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (Lg.lg args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (Lg.lg args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Lg.lg args) []
            result `shouldSatisfy` isLeft
