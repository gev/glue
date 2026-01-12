module Glue.Lib.List.ButlastSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Butlast (butlast)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Butlast (Test butlast function)" do
    it "returns all elements except the last one" do
        let args = [List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (butlast args) []
        case result of
            Left err -> expectationFailure $ "Butlast failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 2]

    it "returns empty list for single-element list" do
        let args = [List [Integer 42]]
        result <- runEvalSimple (butlast args) []
        case result of
            Left err -> expectationFailure $ "Butlast failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "returns string elements except last" do
        let args = [List [String "hello", String "world", String "test"]]
        result <- runEvalSimple (butlast args) []
        case result of
            Left err -> expectationFailure $ "Butlast failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [String "hello", String "world"]

    it "fails on empty list" do
        let args = [List []]
        result <- runEvalSimple (butlast args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Butlast should fail on empty list"

    it "fails on non-list argument" do
        let args = [Integer 42]
        result <- runEvalSimple (butlast args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Butlast should fail on non-list"
