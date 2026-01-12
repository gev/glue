module Glue.Lib.Math.Arithmetic.AddSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Arithmetic.Add qualified as Add
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Arithmetic.Add (Test add function)" do
    describe "Add function" do
        it "returns 5 for (+ 2 3)" do
            let args = [Integer 2, Integer 3]
            result <- runEvalSimple (Add.add args) []
            case result of
                Left err -> expectationFailure $ "Add failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 5

        it "returns 3.5 for (+ 1.5 2)" do
            let args = [Float 1.5, Integer 2]
            result <- runEvalSimple (Add.add args) []
            case result of
                Left err -> expectationFailure $ "Add failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 3.5

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Add.add args) []
            result `shouldSatisfy` isLeft

        it "fails with one argument" do
            let args = [Integer 2]
            result <- runEvalSimple (Add.add args) []
            result `shouldSatisfy` isLeft

        it "fails with three arguments" do
            let args = [Integer 1, Integer 2, Integer 3]
            result <- runEvalSimple (Add.add args) []
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [Integer 1, String "hello"]
            result <- runEvalSimple (Add.add args) []
            result `shouldSatisfy` isLeft

    describe "Type promotion in addition" do
        it "Integer + Integer = Integer" do
            let args = [Integer 5, Integer 3]
            result <- runEvalSimple (Add.add args) []
            case result of
                Left err -> expectationFailure $ "Add failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 8

        it "Integer + Float = Float" do
            let args = [Integer 5, Float 3.5]
            result <- runEvalSimple (Add.add args) []
            case result of
                Left err -> expectationFailure $ "Add failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 8.5

        it "Float + Integer = Float" do
            let args = [Float 5.5, Integer 3]
            result <- runEvalSimple (Add.add args) []
            case result of
                Left err -> expectationFailure $ "Add failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 8.5

        it "Float + Float = Float" do
            let args = [Float 5.5, Float 3.5]
            result <- runEvalSimple (Add.add args) []
            case result of
                Left err -> expectationFailure $ "Add failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 9.0
