module Glue.Lib.Math.Arithmetic.SubSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Arithmetic.Sub qualified as Sub
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Arithmetic.Sub (Test sub function)" do
    describe "Sub function" do
        it "returns -3 for (- 1 4)" do
            let args = [Integer 1, Integer 4]
            result <- runEvalLegacy (Sub.sub args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sub failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer (-3)

        it "returns 2.5 for (- 5.5 3)" do
            let args = [Float 5.5, Integer 3]
            result <- runEvalLegacy (Sub.sub args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sub failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 2.5

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Sub.sub args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with one argument" do
            let args = [Integer 5]
            result <- runEvalLegacy (Sub.sub args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with three arguments" do
            let args = [Integer 10, Integer 4, Integer 4]
            result <- runEvalLegacy (Sub.sub args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [Integer 5, String "hello"]
            result <- runEvalLegacy (Sub.sub args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

    describe "Type promotion in subtraction" do
        it "Integer - Integer = Integer" do
            let args = [Integer 10, Integer 3]
            result <- runEvalLegacy (Sub.sub args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sub failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 7

        it "Integer - Float = Float" do
            let args = [Integer 10, Float 3.5]
            result <- runEvalLegacy (Sub.sub args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sub failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 6.5

        it "Float - Integer = Float" do
            let args = [Float 10.5, Integer 3]
            result <- runEvalLegacy (Sub.sub args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sub failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 7.5

        it "Float - Float = Float" do
            let args = [Float 10.5, Float 3.5]
            result <- runEvalLegacy (Sub.sub args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sub failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 7.0
