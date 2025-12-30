module Reactor.Lib.List.FindSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEvalLegacy)
import Reactor.IR (IR (..), Native (..))
import Reactor.Lib.List.Find qualified as Find
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Find (Test find function)" do
    it "finds first element that satisfies predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ if x > 2 then Symbol "true" else Symbol "false"))
        let args = [pred, List [Number 1, Number 2, Number 3, Number 4]]
        result <- runEvalLegacy (Find.find args) initialEnv
        case result of
            Left err -> expectationFailure $ "Find failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 3

    it "finds first element in list" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ if x > 0 then Symbol "true" else Symbol "false"))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Find.find args) initialEnv
        case result of
            Left err -> expectationFailure $ "Find failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 1

    it "fails when no element satisfies predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ if x > 10 then Symbol "true" else Symbol "false"))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Find.find args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Find should fail when no element found"

    it "fails on empty list" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Symbol "true"))
        let args = [pred, List []]
        result <- runEvalLegacy (Find.find args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Find should fail on empty list"

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Symbol "true"))
        let args = [pred, Number 42]
        result <- runEvalLegacy (Find.find args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Find should fail on non-list"
