module Glue.Lib.List.FindSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..), Native (..))
import Glue.Lib.List.Find qualified as Find
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Find (Test find function)" do
    it "finds first element that satisfies predicate" do
        let pred = Native (Func (\[Integer x] -> pure . Bool $ x > 2))
        let args = [pred, List [Integer 1, Integer 2, Integer 3, Integer 4]]
        result <- runEvalSimple (Find.find args) []
        case result of
            Left err -> expectationFailure $ "Find failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Integer 3

    it "finds first element in list" do
        let pred = Native (Func (\[Integer x] -> pure . Bool $ x > 0))
        let args = [pred, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Find.find args) []
        case result of
            Left err -> expectationFailure $ "Find failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Integer 1

    it "fails when no element satisfies predicate" do
        let pred = Native (Func (\[Integer x] -> pure . Bool $ x > 10))
        let args = [pred, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Find.find args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Find should fail when no element found"

    it "fails on empty list" do
        let pred = Native (Func (\[Integer x] -> pure $ Bool True))
        let args = [pred, List []]
        result <- runEvalSimple (Find.find args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Find should fail on empty list"

    it "fails on non-list second argument" do
        let pred = Native (Func (\[Integer x] -> pure $ Bool True))
        let args = [pred, Integer 42]
        result <- runEvalSimple (Find.find args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Find should fail on non-list"
