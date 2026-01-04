module Glue.Lib.List.FilterSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..), Native (..))
import Glue.Lib.List.Filter qualified as Filter
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Filter (Test filter function)" do
    it "filters elements that satisfy predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure . Bool $ x > 2))
        let args = [pred, List [Number 1, Number 2, Number 3, Number 4]]
        result <- runEvalLegacy (Filter.filter args) initialEnv
        case result of
            Left err -> expectationFailure $ "Filter failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 3, Number 4]

    it "returns empty list when no elements satisfy predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure . Bool $ x > 10))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Filter.filter args) initialEnv
        case result of
            Left err -> expectationFailure $ "Filter failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "returns all elements when all satisfy predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure . Bool $ x > 0))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Filter.filter args) initialEnv
        case result of
            Left err -> expectationFailure $ "Filter failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3]

    it "filters empty list" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Bool True))
        let args = [pred, List []]
        result <- runEvalLegacy (Filter.filter args) initialEnv
        case result of
            Left err -> expectationFailure $ "Filter failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Bool True))
        let args = [pred, Number 42]
        result <- runEvalLegacy (Filter.filter args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Filter should fail on non-list"
