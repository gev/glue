module Reactor.Lib.List.FilterSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..), Native (..))
import Reactor.Lib.List.Filter qualified as Filter
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Filter (Test filter function)" do
    it "filters elements that satisfy predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ if x > 2 then Symbol "true" else Symbol "false"))
        let args = [pred, List [Number 1, Number 2, Number 3, Number 4]]
        result <- runEval (Filter.filter args) initialEnv
        case result of
            Left err -> expectationFailure $ "Filter failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 3, Number 4]

    it "returns empty list when no elements satisfy predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ if x > 10 then Symbol "true" else Symbol "false"))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEval (Filter.filter args) initialEnv
        case result of
            Left err -> expectationFailure $ "Filter failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "returns all elements when all satisfy predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ if x > 0 then Symbol "true" else Symbol "false"))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEval (Filter.filter args) initialEnv
        case result of
            Left err -> expectationFailure $ "Filter failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3]

    it "filters empty list" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Symbol "true"))
        let args = [pred, List []]
        result <- runEval (Filter.filter args) initialEnv
        case result of
            Left err -> expectationFailure $ "Filter failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Symbol "true"))
        let args = [pred, Number 42]
        result <- runEval (Filter.filter args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Filter should fail on non-list"
