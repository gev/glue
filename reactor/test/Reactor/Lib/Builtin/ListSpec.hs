module Reactor.Lib.Builtin.ListSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib.Builtin.List (car, list)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Builtin.List (Test list function)" do
    describe "Creating lists" do
        it "creates a list from arguments" do
            let initialEnv = E.emptyEnv
            let args = [Number 1, Number 2, Number 3]
            result <- runEval (list args) initialEnv
            case result of
                Left err -> expectationFailure $ "List failed: " <> show err
                Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3]

        it "creates an empty list" do
            let initialEnv = E.emptyEnv
            let args = []
            result <- runEval (list args) initialEnv
            case result of
                Left err -> expectationFailure $ "List failed: " <> show err
                Right (res, _, _) -> res `shouldBe` List []

        it "creates a list with mixed types" do
            let initialEnv = E.emptyEnv
            let args = [Number 42, String "hello", Symbol "x"]
            result <- runEval (list args) initialEnv
            case result of
                Left err -> expectationFailure $ "List failed: " <> show err
                Right (res, _, _) -> res `shouldBe` List [Number 42, String "hello", Symbol "x"]

    describe "car function" do
        it "returns the first element of a list" do
            let initialEnv = E.emptyEnv
            let args = [List [Number 1, Number 2, Number 3]]
            result <- runEval (car args) initialEnv
            case result of
                Left err -> expectationFailure $ "Car failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 1

        it "fails on empty list" do
            let initialEnv = E.emptyEnv
            let args = [List []]
            result <- runEval (car args) initialEnv
            case result of
                Left _ -> pure () -- Expected error
                Right _ -> expectationFailure "Car should fail on empty list"

        it "fails on non-list" do
            let initialEnv = E.emptyEnv
            let args = [Number 42]
            result <- runEval (car args) initialEnv
            case result of
                Left _ -> pure () -- Expected error
                Right _ -> expectationFailure "Car should fail on non-list"
