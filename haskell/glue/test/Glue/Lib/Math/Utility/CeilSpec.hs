module Glue.Lib.Math.Utility.CeilSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Utility.Ceil qualified as Ceil
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Utility.Ceil (Test ceil function)" do
    describe "Ceil function" do
        it "returns 4 for ceil(3.1)" do
            let args = [Float 3.1]
            result <- runEvalSimple (Ceil.ceil args) []
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 4

        it "returns 3 for ceil(2.9)" do
            let args = [Float 2.9]
            result <- runEvalSimple (Ceil.ceil args) []
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 3

        it "returns -3 for ceil(-3.1)" do
            let args = [Float (-3.1)]
            result <- runEvalSimple (Ceil.ceil args) []
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer (-3)

        it "returns 5 for ceil(5.0)" do
            let args = [Integer 5]
            result <- runEvalSimple (Ceil.ceil args) []
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 5

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (Ceil.ceil args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (Ceil.ceil args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Ceil.ceil args) []
            result `shouldSatisfy` isLeft
