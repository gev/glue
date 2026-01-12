module Glue.Lib.List.RemoveSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Remove (remove)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Remove (Test remove function)" do
    it "removes item from list" do
        let args = [Integer 2, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (remove args) []
        case result of
            Left err -> expectationFailure $ "Remove failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 3]

    it "removes all occurrences of item" do
        let args = [Integer 2, List [Integer 1, Integer 2, Integer 2, Integer 3]]
        result <- runEvalSimple (remove args) []
        case result of
            Left err -> expectationFailure $ "Remove failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 3]

    it "returns same list if item not found" do
        let args = [Integer 4, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (remove args) []
        case result of
            Left err -> expectationFailure $ "Remove failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3]

    it "removes from empty list" do
        let args = [Integer 1, List []]
        result <- runEvalSimple (remove args) []
        case result of
            Left err -> expectationFailure $ "Remove failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "fails on non-list second argument" do
        let args = [Integer 1, Integer 42]
        result <- runEvalSimple (remove args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Remove should fail on non-list"
