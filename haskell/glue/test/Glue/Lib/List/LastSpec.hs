module Glue.Lib.List.LastSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Last qualified as Last
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Last (Test last function)" do
    it "returns the last element of a list" do
        let args = [List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Last.last args) []
        case result of
            Left err -> expectationFailure $ "Last failed: " <> show err
            Right (res, _) -> res `shouldBe` Integer 3

    it "returns the only element of a single-element list" do
        let args = [List [Integer 42]]
        result <- runEvalSimple (Last.last args) []
        case result of
            Left err -> expectationFailure $ "Last failed: " <> show err
            Right (res, _) -> res `shouldBe` Integer 42

    it "returns string element" do
        let args = [List [String "hello", String "world"]]
        result <- runEvalSimple (Last.last args) []
        case result of
            Left err -> expectationFailure $ "Last failed: " <> show err
            Right (res, _) -> res `shouldBe` String "world"

    it "fails on empty list" do
        let args = [List []]
        result <- runEvalSimple (Last.last args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Last should fail on empty list"

    it "fails on non-list argument" do
        let args = [Integer 42]
        result <- runEvalSimple (Last.last args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Last should fail on non-list"
