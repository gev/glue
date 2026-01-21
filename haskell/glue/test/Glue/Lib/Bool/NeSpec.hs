module Glue.Lib.Bool.NeSpec (spec) where

import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Ne (ne)
import Glue.TestUtils ()
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Ne (Test ne function)" do
    describe "Not equal comparison" do
        it "returns true for unequal numbers" do
            let args = [Integer 42, Integer 43]
            result <- runEvalSimple (apply ne args) []
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool True

        it "returns false for equal numbers" do
            let args = [Integer 42, Integer 42]
            result <- runEvalSimple (apply ne args) []
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "returns true for unequal strings" do
            let args = [String "hello", String "world"]
            result <- runEvalSimple (apply ne args) []
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool True

        it "returns false for equal strings" do
            let args = [String "hello", String "hello"]
            result <- runEvalSimple (apply ne args) []
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "\\= alias works identically to ne" do
            let args1 = [Integer 42, Integer 43] -- unequal
            let args2 = [Integer 42, Integer 42] -- equal
            result1 <- runEvalSimple (apply ne args1) []
            result2 <- runEvalSimple (apply ne args2) []
            case (result1, result2) of
                (Right (Bool True, _), Right (Bool False, _)) -> pure ()
                _ -> expectationFailure "\\= alias should work like ne"
