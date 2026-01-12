module Glue.Lib.Math.Power.PowSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Power.Pow qualified as Pow
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Power.Pow (Test pow function)" do
    describe "Power function" do
        it "returns 2^3 = 8" do
            let args = [Integer 2, Integer 3]
            result <- runEvalSimple (Pow.pow args) []
            case result of
                Left err -> expectationFailure $ "Pow failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 8

        it "returns 3^2 = 9" do
            let args = [Integer 3, Integer 2]
            result <- runEvalSimple (Pow.pow args) []
            case result of
                Left err -> expectationFailure $ "Pow failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 9

        it "returns 2^0 = 1" do
            let args = [Integer 2, Integer 0]
            result <- runEvalSimple (Pow.pow args) []
            case result of
                Left err -> expectationFailure $ "Pow failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 1

        it "returns 0^5 = 0" do
            let args = [Integer 0, Integer 5]
            result <- runEvalSimple (Pow.pow args) []
            case result of
                Left err -> expectationFailure $ "Pow failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 0

        it "fails with non-numbers (first arg)" do
            let args = [String "hello", Integer 2]
            result <- runEvalSimple (Pow.pow args) []
            result `shouldSatisfy` isLeft

        it "fails with non-numbers (second arg)" do
            let args = [Integer 2, String "hello"]
            result <- runEvalSimple (Pow.pow args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (one)" do
            let args = [Integer 2]
            result <- runEvalSimple (Pow.pow args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (three)" do
            let args = [Integer 2, Integer 3, Integer 4]
            result <- runEvalSimple (Pow.pow args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Pow.pow args) []
            result `shouldSatisfy` isLeft
