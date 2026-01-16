module Glue.Lib.Math.Utility.TruncSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Utility.Trunc qualified as Trunc
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Utility.Trunc (Test trunc function)" do
    describe "Trunc function" do
        it "returns 3 for trunc(3.7)" do
            let args = [Float 3.7]
            result <- runEvalSimple (Trunc.trunc args) []
            case result of
                Left err -> expectationFailure $ "Trunc failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 3

        it "returns 2 for trunc(2.1)" do
            let args = [Float 2.1]
            result <- runEvalSimple (Trunc.trunc args) []
            case result of
                Left err -> expectationFailure $ "Trunc failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 2

        it "returns -3 for trunc(-3.7)" do
            let args = [Float (-3.7)]
            result <- runEvalSimple (Trunc.trunc args) []
            case result of
                Left err -> expectationFailure $ "Trunc failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer (-3)

        it "returns 5 for trunc(5.0)" do
            let args = [Integer 5]
            result <- runEvalSimple (Trunc.trunc args) []
            case result of
                Left err -> expectationFailure $ "Trunc failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 5

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (Trunc.trunc args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (Trunc.trunc args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Trunc.trunc args) []
            result `shouldSatisfy` isLeft
