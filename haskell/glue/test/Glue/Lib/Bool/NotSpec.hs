module Glue.Lib.Bool.NotSpec (spec) where

import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Not (not_)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Not (Test not function)" do
    describe "Logical negation" do
        it "returns false for true" do
            result <- runEvalSimple (apply not_ [Bool True]) []
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "returns true for false" do
            result <- runEvalSimple (apply not_ [Bool False]) []
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool True

        it "returns false for other values" do
            result <- runEvalSimple (apply not_ [Integer 42]) []
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "! alias works identically to not" do
            result1 <- runEvalSimple (apply not_ [Bool False]) [] -- not false = true
            result2 <- runEvalSimple (apply not_ [Bool True]) [] -- not true = false
            result3 <- runEvalSimple (apply not_ [Integer 42]) [] -- not 42 = false (truthy)
            case (result1, result2, result3) of
                (Right (Bool True, _), Right (Bool False, _), Right (Bool False, _)) -> pure ()
                _ -> expectationFailure "! alias should work like not"
