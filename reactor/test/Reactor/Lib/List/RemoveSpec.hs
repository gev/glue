module Reactor.Lib.List.RemoveSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib.List.Remove (remove)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Remove (Test remove function)" do
    it "removes item from list" do
        let initialEnv = E.emptyEnv
        let args = [Number 2, List [Number 1, Number 2, Number 3]]
        result <- runEval (remove args) initialEnv
        case result of
            Left err -> expectationFailure $ "Remove failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 3]

    it "removes all occurrences of item" do
        let initialEnv = E.emptyEnv
        let args = [Number 2, List [Number 1, Number 2, Number 2, Number 3]]
        result <- runEval (remove args) initialEnv
        case result of
            Left err -> expectationFailure $ "Remove failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 3]

    it "returns same list if item not found" do
        let initialEnv = E.emptyEnv
        let args = [Number 4, List [Number 1, Number 2, Number 3]]
        result <- runEval (remove args) initialEnv
        case result of
            Left err -> expectationFailure $ "Remove failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3]

    it "removes from empty list" do
        let initialEnv = E.emptyEnv
        let args = [Number 1, List []]
        result <- runEval (remove args) initialEnv
        case result of
            Left err -> expectationFailure $ "Remove failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let args = [Number 1, Number 42]
        result <- runEval (remove args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Remove should fail on non-list"
