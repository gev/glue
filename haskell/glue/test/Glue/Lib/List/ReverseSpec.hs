module Glue.Lib.List.ReverseSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Reverse qualified as Reverse
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Reverse (Test reverse function)" do
    it "reverses a list" do
        let args = [List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Reverse.reverse args) []
        case result of
            Left err -> expectationFailure $ "Reverse failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 3, Integer 2, Integer 1]

    it "reverses an empty list" do
        let args = [List []]
        result <- runEvalSimple (Reverse.reverse args) []
        case result of
            Left err -> expectationFailure $ "Reverse failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "reverses a single element list" do
        let args = [List [String "hello"]]
        result <- runEvalSimple (Reverse.reverse args) []
        case result of
            Left err -> expectationFailure $ "Reverse failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [String "hello"]

    it "reverses a single element is a list" do
        let args = [List [List [String "hello"]]]
        result <- runEvalSimple (Reverse.reverse args) []
        case result of
            Left err -> expectationFailure $ "Reverse failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [String "hello"]]

    it "fails on non-list" do
        let args = [Integer 42]
        result <- runEvalSimple (Reverse.reverse args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Reverse should fail on non-list"
