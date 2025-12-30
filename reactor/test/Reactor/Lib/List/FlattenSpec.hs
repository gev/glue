module Reactor.Lib.List.FlattenSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEvalLegacy)
import Reactor.IR (IR (..))
import Reactor.Lib.List.Flatten qualified as Flatten
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Flatten (Test flatten function)" do
    it "flattens a simple nested list" do
        let initialEnv = E.emptyEnv
        let args = [List [List [Number 1, Number 2], List [Number 3, Number 4]]]
        result <- runEvalLegacy (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3, Number 4]

    it "flattens deeply nested lists" do
        let initialEnv = E.emptyEnv
        let args = [List [List [List [Number 1], Number 2], Number 3]]
        result <- runEvalLegacy (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3]

    it "flattens list with mixed elements" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, List [Number 2, Number 3], Number 4]]
        result <- runEvalLegacy (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3, Number 4]

    it "flattens empty list" do
        let initialEnv = E.emptyEnv
        let args = [List []]
        result <- runEvalLegacy (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "flattens list with empty sublists" do
        let initialEnv = E.emptyEnv
        let args = [List [List [], Number 1, List []]]
        result <- runEvalLegacy (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1]

    it "flattens single element list" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 42]]
        result <- runEvalLegacy (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 42]

    it "fails on non-list argument" do
        let initialEnv = E.emptyEnv
        let args = [Number 42]
        result <- runEvalLegacy (Flatten.flatten args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Flatten should fail on non-list"
