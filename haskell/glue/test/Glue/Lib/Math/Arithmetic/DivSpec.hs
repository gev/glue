module Glue.Lib.Math.Arithmetic.DivSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Arithmetic.Div qualified as Div
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Arithmetic.Div (Test div function)" do
    describe "Div function" do
        it "returns 2.0 for (/ 10 5)" do
            let args = [Integer 10, Integer 5]
            result <- runEvalSimple (Div.div args) []
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 2.0

        it "returns 2.5 for (/ 10 4)" do
            let args = [Integer 10, Integer 4]
            result <- runEvalSimple (Div.div args) []
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 2.5

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Div.div args) []
            result `shouldSatisfy` isLeft

        it "fails with one argument" do
            let args = [Integer 10]
            result <- runEvalSimple (Div.div args) []
            result `shouldSatisfy` isLeft

        it "fails with three arguments" do
            let args = [Integer 10, Integer 2, Integer 2]
            result <- runEvalSimple (Div.div args) []
            result `shouldSatisfy` isLeft

        it "returns Infinity for division by zero" do
            let args = [Integer 10, Integer 0]
            result <- runEvalSimple (Div.div args) []
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> case res of
                    Float f | f == (1 / 0) -> f `shouldBe` (1 / 0)
                    _ -> expectationFailure "Expected Infinity"

        it "fails with non-numbers" do
            let args = [Integer 10, String "hello"]
            result <- runEvalSimple (Div.div args) []
            result `shouldSatisfy` isLeft

    describe "Type promotion in division" do
        it "Integer / Integer = Float" do
            let args = [Integer 10, Integer 5]
            result <- runEvalSimple (Div.div args) []
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 2.0

        it "Integer / Float = Float" do
            let args = [Integer 10, Float 2.5]
            result <- runEvalSimple (Div.div args) []
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 4.0

        it "Float / Integer = Float" do
            let args = [Float 10.5, Integer 3]
            result <- runEvalSimple (Div.div args) []
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 3.5

        it "Float / Float = Float" do
            let args = [Float 10.5, Float 2.5]
            result <- runEvalSimple (Div.div args) []
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 4.2

        it "returns -Infinity for negative division by zero" do
            let args = [Integer (-1), Integer 0]
            result <- runEvalSimple (Div.div args) []
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> case res of
                    Float f | f == (-1 / 0) -> f `shouldBe` (-1 / 0)
                    _ -> expectationFailure "Expected -Infinity"
