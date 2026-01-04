module Glue.Lib.List.FindSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..), Native (..))
import Glue.Lib.List.Find qualified as Find
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Find (Test find function)" do
    it "finds first element that satisfies predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure . Bool $ x > 2))
        let args = [pred, List [Number 1, Number 2, Number 3, Number 4]]
        result <- runEvalLegacy (Find.find args) initialEnv
        case result of
            Left err -> expectationFailure $ "Find failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 3

    it "finds first element in list" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure . Bool $ x > 0))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Find.find args) initialEnv
        case result of
            Left err -> expectationFailure $ "Find failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 1

    it "fails when no element satisfies predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure . Bool $ x > 10))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Find.find args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Find should fail when no element found"

    it "fails on empty list" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Bool True))
        let args = [pred, List []]
        result <- runEvalLegacy (Find.find args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Find should fail on empty list"

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Bool True))
        let args = [pred, Number 42]
        result <- runEvalLegacy (Find.find args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Find should fail on non-list"
