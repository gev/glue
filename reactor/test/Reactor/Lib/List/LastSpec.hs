module Reactor.Lib.List.LastSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib.List.Last qualified as Last
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Last (Test last function)" do
    it "returns the last element of a list" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, Number 2, Number 3]]
        result <- runEval (Last.last args) initialEnv
        case result of
            Left err -> expectationFailure $ "Last failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 3

    it "returns the only element of a single-element list" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 42]]
        result <- runEval (Last.last args) initialEnv
        case result of
            Left err -> expectationFailure $ "Last failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 42

    it "returns string element" do
        let initialEnv = E.emptyEnv
        let args = [List [String "hello", String "world"]]
        result <- runEval (Last.last args) initialEnv
        case result of
            Left err -> expectationFailure $ "Last failed: " <> show err
            Right (res, _, _) -> res `shouldBe` String "world"

    it "fails on empty list" do
        let initialEnv = E.emptyEnv
        let args = [List []]
        result <- runEval (Last.last args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Last should fail on empty list"

    it "fails on non-list argument" do
        let initialEnv = E.emptyEnv
        let args = [Number 42]
        result <- runEval (Last.last args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Last should fail on non-list"
