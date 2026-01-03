module Glue.Lib.List.NthSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib.List.Nth (nth)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Nth (Test nth function)" do
    it "returns element at index 0" do
        let initialEnv = E.emptyEnv
        let args = [Number 0, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (nth args) initialEnv
        case result of
            Left err -> expectationFailure $ "Nth failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 1

    it "returns element at index 1" do
        let initialEnv = E.emptyEnv
        let args = [Number 1, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (nth args) initialEnv
        case result of
            Left err -> expectationFailure $ "Nth failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 2

    it "returns element at last index" do
        let initialEnv = E.emptyEnv
        let args = [Number 2, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (nth args) initialEnv
        case result of
            Left err -> expectationFailure $ "Nth failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 3

    it "fails on negative index" do
        let initialEnv = E.emptyEnv
        let args = [Number (-1), List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (nth args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Nth should fail on negative index"

    it "fails on index out of bounds" do
        let initialEnv = E.emptyEnv
        let args = [Number 3, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (nth args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Nth should fail on index out of bounds"

    it "fails on non-number index" do
        let initialEnv = E.emptyEnv
        let args = [String "0", List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (nth args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Nth should fail on non-number index"

    it "fails on non-list" do
        let initialEnv = E.emptyEnv
        let args = [Number 0, Number 42]
        result <- runEvalLegacy (nth args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Nth should fail on non-list"
