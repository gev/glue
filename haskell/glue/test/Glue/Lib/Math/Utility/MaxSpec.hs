module Glue.Lib.Math.Utility.MaxSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Utility.Max qualified as Max
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Utility.Max (Test max function)" do
    describe "Maximum function" do
        it "returns 5 for max(2, 5)" do
            let args = [Integer 2, Integer 5]
            result <- runEvalSimple (Max.max args) []
            case result of
                Left err -> expectationFailure $ "Max failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 5

        it "returns 3 for max(3, 3)" do
            let args = [Integer 3, Integer 3]
            result <- runEvalSimple (Max.max args) []
            case result of
                Left err -> expectationFailure $ "Max failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 3

        it "returns -2 for max(-5, -2)" do
            let args = [Float (-5), Float (-2)]
            result <- runEvalSimple (Max.max args) []
            case result of
                Left err -> expectationFailure $ "Max failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float (-2)

        it "returns 10 for max(1, 10)" do
            let args = [Integer 1, Integer 10]
            result <- runEvalSimple (Max.max args) []
            case result of
                Left err -> expectationFailure $ "Max failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 10

        it "fails with non-numbers (first arg)" do
            let args = [String "hello", Integer 2]
            result <- runEvalSimple (Max.max args) []
            result `shouldSatisfy` isLeft

        it "fails with non-numbers (second arg)" do
            let args = [Integer 2, String "hello"]
            result <- runEvalSimple (Max.max args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (one)" do
            let args = [Integer 2]
            result <- runEvalSimple (Max.max args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (three)" do
            let args = [Integer 2, Integer 3, Integer 4]
            result <- runEvalSimple (Max.max args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Max.max args) []
            result `shouldSatisfy` isLeft
