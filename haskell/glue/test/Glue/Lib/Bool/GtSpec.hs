module Glue.Lib.Bool.GtSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Gt (gt)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Gt (Test gt function)" do
    describe "Greater than comparison" do
        it "returns true for greater number" do
            let args = [Integer 10, Integer 5]
            result <- runEvalSimple (apply gt args) []
            case result of
                Left err -> expectationFailure $ "Gt failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool True

        it "returns false for equal numbers" do
            let args = [Integer 5, Integer 5]
            result <- runEvalSimple (apply gt args) []
            case result of
                Left err -> expectationFailure $ "Gt failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "returns false for lesser number" do
            let args = [Integer 5, Integer 10]
            result <- runEvalSimple (apply gt args) []
            case result of
                Left err -> expectationFailure $ "Gt failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "fails with non-numbers" do
            let args = [String "hello", String "world"]
            result <- runEvalSimple (apply gt args) []
            result `shouldSatisfy` isLeft

        it "> alias works identically to gt" do
            let args1 = [Integer 10, Integer 5] -- greater
            let args2 = [Integer 5, Integer 5] -- equal
            let args3 = [Integer 5, Integer 10] -- lesser
            result1 <- runEvalSimple (apply gt args1) []
            result2 <- runEvalSimple (apply gt args2) []
            result3 <- runEvalSimple (apply gt args3) []
            case (result1, result2, result3) of
                (Right (Bool True, _), Right (Bool False, _), Right (Bool False, _)) -> pure ()
                _ -> expectationFailure "> alias should work like gt"
