module Glue.Lib.List.TakeSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Take qualified as Take
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Take (Test take function)" do
    it "takes first N elements from list" do
        let args = [Integer 3, List [Integer 1, Integer 2, Integer 3, Integer 4, Integer 5]]
        result <- runEvalSimple (Take.take args) []
        case result of
            Left err -> expectationFailure $ "Take failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3]

    it "takes fewer elements when N > list length" do
        let args = [Integer 10, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Take.take args) []
        case result of
            Left err -> expectationFailure $ "Take failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3]

    it "takes zero elements" do
        let args = [Integer 0, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Take.take args) []
        case result of
            Left err -> expectationFailure $ "Take failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "takes all elements when N equals list length" do
        let args = [Integer 3, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Take.take args) []
        case result of
            Left err -> expectationFailure $ "Take failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3]

    it "fails on negative count" do
        let args = [Float (-1), List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Take.take args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Take should fail on negative count"

    it "fails on non-number first argument" do
        let args = [String "3", List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Take.take args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Take should fail on non-number count"

    it "fails on non-list second argument" do
        let args = [Integer 3, Integer 42]
        result <- runEvalSimple (Take.take args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Take should fail on non-list"
