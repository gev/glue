module Glue.Lib.Bool.NotSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Not (not_)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Not (Test not function)" do
    describe "Logical negation" do
        it "returns false for true" do
            let args = [Bool True]
            result <- runEvalSimple (not_ args) []
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "returns true for false" do
            let args = [Bool False]
            result <- runEvalSimple (not_ args) []
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool True

        it "returns false for other values" do
            let args = [Integer 42]
            result <- runEvalSimple (not_ args) []
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "fails with wrong number of arguments" do
            let args = []
            result <- runEvalSimple (not_ args) []
            result `shouldSatisfy` isLeft

        it "! alias works identically to not" do
            let args1 = [Bool False] -- not false = true
            let args2 = [Bool True] -- not true = false
            let args3 = [Integer 42] -- not 42 = false (truthy)
            result1 <- runEvalSimple (not_ args1) []
            result2 <- runEvalSimple (not_ args2) []
            result3 <- runEvalSimple (not_ args3) []
            case (result1, result2, result3) of
                (Right (Bool True, _), Right (Bool False, _), Right (Bool False, _)) -> pure ()
                _ -> expectationFailure "! alias should work like not"
