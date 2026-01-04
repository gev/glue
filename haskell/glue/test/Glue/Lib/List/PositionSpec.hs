module Glue.Lib.List.PositionSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..), Native (..))
import Glue.Lib.List.Position qualified as Position
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Position (Test position function)" do
    it "finds position of first element that satisfies predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure . Bool $ x > 2))
        let args = [pred, List [Number 1, Number 2, Number 3, Number 4]]
        result <- runEvalLegacy (Position.position args) initialEnv
        case result of
            Left err -> expectationFailure $ "Position failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 2

    it "finds position of first element in list" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure . Bool $ x > 0))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Position.position args) initialEnv
        case result of
            Left err -> expectationFailure $ "Position failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 0

    it "finds position of element in middle of list" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure . Bool $ x == 5))
        let args = [pred, List [Number 1, Number 2, Number 5, Number 3]]
        result <- runEvalLegacy (Position.position args) initialEnv
        case result of
            Left err -> expectationFailure $ "Position failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 2

    it "fails when no element satisfies predicate" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure . Bool $ x > 10))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Position.position args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Position should fail when no element found"

    it "fails on empty list" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Bool True))
        let args = [pred, List []]
        result <- runEvalLegacy (Position.position args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Position should fail on empty list"

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Bool True))
        let args = [pred, Number 42]
        result <- runEvalLegacy (Position.position args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Position should fail on non-list"
