module Glue.Lib.List.SortSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Sort qualified as Sort
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Sort (Test sort function)" do
    it "sorts a list of numbers in ascending order" do
        let args = [List [Integer 3, Integer 1, Integer 4, Integer 1, Integer 5]]
        result <- runEvalSimple (Sort.sort args) []
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Integer 1, Integer 1, Integer 3, Integer 4, Integer 5]

    it "sorts a list of strings in alphabetical order" do
        let args = [List [String "zebra", String "apple", String "banana"]]
        result <- runEvalSimple (Sort.sort args) []
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _) -> res `shouldBe` List [String "apple", String "banana", String "zebra"]

    it "sorts a list of symbols in alphabetical order" do
        let args = [List [Symbol "zebra", Symbol "apple", Symbol "banana"]]
        result <- runEvalSimple (Sort.sort args) []
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Symbol "apple", Symbol "banana", Symbol "zebra"]

    it "sorts an empty list" do
        let args = [List []]
        result <- runEvalSimple (Sort.sort args) []
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _) -> res `shouldBe` List []

    it "sorts a single element list" do
        let args = [List [Integer 42]]
        result <- runEvalSimple (Sort.sort args) []
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Integer 42]

    it "sorts a list with duplicate elements" do
        let args = [List [Integer 3, Integer 1, Integer 3, Integer 1, Integer 2]]
        result <- runEvalSimple (Sort.sort args) []
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Integer 1, Integer 1, Integer 2, Integer 3, Integer 3]

    it "fails on non-list argument" do
        let args = [Integer 42]
        result <- runEvalSimple (Sort.sort args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Sort should fail on non-list"

    it "fails on list with incomparable elements" do
        let args = [List [Integer 1, List [Integer 2]]]
        result <- runEvalSimple (Sort.sort args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Sort should fail on incomparable elements"
