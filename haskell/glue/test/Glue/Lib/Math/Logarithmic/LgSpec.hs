module Glue.Lib.Math.Logarithmic.LgSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Logarithmic.Lg qualified as Lg
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Logarithmic.Lg (Test lg function)" do
    describe "Common logarithm function (base 10)" do
        it "returns lg(1) = 0" do
            let args = [Integer 1]
            result <- runEvalLegacy (Lg.lg args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lg failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs x < 1e-10)
                    _ -> expectationFailure "Expected a Float"

        it "returns lg(10) = 1" do
            let args = [Integer 10]
            result <- runEvalLegacy (Lg.lg args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lg failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 1) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns lg(100) = 2" do
            let args = [Integer 100]
            result <- runEvalLegacy (Lg.lg args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lg failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 2) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "fails with zero" do
            let args = [Integer 0]
            result <- runEvalLegacy (Lg.lg args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with negative numbers" do
            let args = [Float (-1)]
            result <- runEvalLegacy (Lg.lg args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalLegacy (Lg.lg args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalLegacy (Lg.lg args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Lg.lg args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
