module Glue.Lib.Bool.LeSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Le (le)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Le (Test le function)" do
    describe "Less than or equal comparison" do
        it "returns true for equal numbers" do
            let args = [Integer 5, Integer 5]
            result <- runEvalSimple (le args) []
            case result of
                Left err -> expectationFailure $ "Le failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns true for lesser number" do
            let args = [Integer 5, Integer 10]
            result <- runEvalSimple (le args) []
            case result of
                Left err -> expectationFailure $ "Le failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns false for greater number" do
            let args = [Integer 10, Integer 5]
            result <- runEvalSimple (le args) []
            case result of
                Left err -> expectationFailure $ "Le failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "fails with non-numbers" do
            let args = [String "hello", String "world"]
            result <- runEvalSimple (le args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 5]
            result <- runEvalSimple (le args) []
            result `shouldSatisfy` isLeft

        it "<= alias works identically to le" do
            let args1 = [Integer 5, Integer 5] -- equal
            let args2 = [Integer 5, Integer 10] -- lesser
            let args3 = [Integer 10, Integer 5] -- greater
            result1 <- runEvalSimple (le args1) []
            result2 <- runEvalSimple (le args2) []
            result3 <- runEvalSimple (le args3) []
            case (result1, result2, result3) of
                (Right (Bool True, _, _), Right (Bool True, _, _), Right (Bool False, _, _)) -> pure ()
                _ -> expectationFailure "<= alias should work like le"
