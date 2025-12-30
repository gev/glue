module Reactor.Lib.List.TakeSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEvalLegacy)
import Reactor.IR (IR (..))
import Reactor.Lib.List.Take qualified as Take
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Take (Test take function)" do
    it "takes first N elements from list" do
        let initialEnv = E.emptyEnv
        let args = [Number 3, List [Number 1, Number 2, Number 3, Number 4, Number 5]]
        result <- runEvalLegacy (Take.take args) initialEnv
        case result of
            Left err -> expectationFailure $ "Take failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3]

    it "takes fewer elements when N > list length" do
        let initialEnv = E.emptyEnv
        let args = [Number 10, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Take.take args) initialEnv
        case result of
            Left err -> expectationFailure $ "Take failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3]

    it "takes zero elements" do
        let initialEnv = E.emptyEnv
        let args = [Number 0, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Take.take args) initialEnv
        case result of
            Left err -> expectationFailure $ "Take failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "takes all elements when N equals list length" do
        let initialEnv = E.emptyEnv
        let args = [Number 3, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Take.take args) initialEnv
        case result of
            Left err -> expectationFailure $ "Take failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3]

    it "fails on negative count" do
        let initialEnv = E.emptyEnv
        let args = [Number (-1), List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Take.take args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Take should fail on negative count"

    it "fails on non-number first argument" do
        let initialEnv = E.emptyEnv
        let args = [String "3", List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Take.take args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Take should fail on non-number count"

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let args = [Number 3, Number 42]
        result <- runEvalLegacy (Take.take args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Take should fail on non-list"
