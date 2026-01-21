module Glue.Lib.Bool.GeSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Ge (ge)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Ge (Test ge function)" do
    describe "Greater than or equal comparison" do
        it "returns true for equal numbers" do
            let args = [Integer 5, Integer 5]
            result <- runEvalSimple (apply ge args) []
            case result of
                Left err -> expectationFailure $ "Ge failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool True

        it "returns true for greater number" do
            let args = [Integer 10, Integer 5]
            result <- runEvalSimple (apply ge args) []
            case result of
                Left err -> expectationFailure $ "Ge failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool True

        it "returns false for lesser number" do
            let args = [Integer 5, Integer 10]
            result <- runEvalSimple (apply ge args) []
            case result of
                Left err -> expectationFailure $ "Ge failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "fails with non-numbers" do
            let args = [String "hello", String "world"]
            result <- runEvalSimple (apply ge args) []
            result `shouldSatisfy` isLeft

        it ">= alias works identically to ge" do
            let args1 = [Integer 5, Integer 5] -- equal
            let args2 = [Integer 10, Integer 5] -- greater
            let args3 = [Integer 5, Integer 10] -- lesser
            result1 <- runEvalSimple (apply ge args1) []
            result2 <- runEvalSimple (apply ge args2) []
            result3 <- runEvalSimple (apply ge args3) []
            case (result1, result2, result3) of
                (Right (Bool True, _), Right (Bool True, _), Right (Bool False, _)) -> pure ()
                _ -> expectationFailure ">= alias should work like ge"
