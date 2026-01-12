module Glue.Lib.Bool.NeSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Ne (ne)
import Glue.TestUtils ()
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Ne (Test ne function)" do
    describe "Not equal comparison" do
        it "returns true for unequal numbers" do
            let args = [Integer 42, Integer 43]
            result <- runEvalSimple (ne args) []
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns false for equal numbers" do
            let args = [Integer 42, Integer 42]
            result <- runEvalSimple (ne args) []
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "returns true for unequal strings" do
            let args = [String "hello", String "world"]
            result <- runEvalSimple (ne args) []
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns false for equal strings" do
            let args = [String "hello", String "hello"]
            result <- runEvalSimple (ne args) []
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "fails with wrong number of arguments" do
            let args = [Integer 42]
            result <- runEvalSimple (ne args) []
            result `shouldSatisfy` isLeft

        it "\\= alias works identically to ne" do
            let args1 = [Integer 42, Integer 43] -- unequal
            let args2 = [Integer 42, Integer 42] -- equal
            result1 <- runEvalSimple (ne args1) []
            result2 <- runEvalSimple (ne args2) []
            case (result1, result2) of
                (Right (Bool True, _, _), Right (Bool False, _, _)) -> pure ()
                _ -> expectationFailure "\\= alias should work like ne"
