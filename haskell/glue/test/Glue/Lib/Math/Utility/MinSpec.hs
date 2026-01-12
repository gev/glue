module Glue.Lib.Math.Utility.MinSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Utility.Min qualified as Min
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Utility.Min (Test min function)" do
    describe "Minimum function" do
        it "returns 2 for min(2, 5)" do
            let args = [Integer 2, Integer 5]
            result <- runEvalSimple (Min.min args) []
            case result of
                Left err -> expectationFailure $ "Min failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 2

        it "returns 3 for min(3, 3)" do
            let args = [Integer 3, Integer 3]
            result <- runEvalSimple (Min.min args) []
            case result of
                Left err -> expectationFailure $ "Min failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 3

        it "returns -5 for min(-5, -2)" do
            let args = [Float (-5), Float (-2)]
            result <- runEvalSimple (Min.min args) []
            case result of
                Left err -> expectationFailure $ "Min failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float (-5)

        it "returns 1 for min(1, 10)" do
            let args = [Integer 1, Integer 10]
            result <- runEvalSimple (Min.min args) []
            case result of
                Left err -> expectationFailure $ "Min failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 1

        it "fails with non-numbers (first arg)" do
            let args = [String "hello", Integer 2]
            result <- runEvalSimple (Min.min args) []
            result `shouldSatisfy` isLeft

        it "fails with non-numbers (second arg)" do
            let args = [Integer 2, String "hello"]
            result <- runEvalSimple (Min.min args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (one)" do
            let args = [Integer 2]
            result <- runEvalSimple (Min.min args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (three)" do
            let args = [Integer 2, Integer 3, Integer 4]
            result <- runEvalSimple (Min.min args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Min.min args) []
            result `shouldSatisfy` isLeft
