module Glue.Lib.Math.Logarithmic.LogSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Logarithmic.Log qualified as Log
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Logarithmic.Log (Test log function)" do
    describe "Logarithm function with base" do
        it "returns log(1, e) = 0" do
            let args = [Integer 1, Float (exp 1)]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Log failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 0

        it "returns log(e, e) = 1" do
            let e = exp (1.0 :: Double)
            let args = [Float e, Float e]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Log failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 1) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns log(e^2, e) = 2" do
            let e = exp (1.0 :: Double)
            let e2 = exp (2.0 :: Double)
            let args = [Float e2, Float e]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Log failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 2) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns log(100, 10) = 2" do
            let args = [Integer 100, Integer 10]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Log failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 2

        it "returns log(8, 2) = 3" do
            let args = [Integer 8, Integer 2]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Log failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 3

        it "fails with zero value" do
            let args = [Integer 0, Integer 10]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with negative value" do
            let args = [Float (-1), Integer 10]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with zero base" do
            let args = [Integer 10, Integer 0]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with negative base" do
            let args = [Integer 10, Float (-1)]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with base 1" do
            let args = [Integer 10, Integer 1]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers (value)" do
            let args = [String "hello", Integer 10]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers (base)" do
            let args = [Integer 10, String "hello"]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (one)" do
            let args = [Integer 1]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (three)" do
            let args = [Integer 1, Integer 2, Integer 3]
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
