module Reactor.Lib.List.CdrSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib.List.Cdr (cdr)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Cdr (Test cdr function)" do
    it "returns the rest of a list" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, Number 2, Number 3]]
        result <- runEval (cdr args) initialEnv
        case result of
            Left err -> expectationFailure $ "Cdr failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 2, Number 3]

    it "fails on empty list" do
        let initialEnv = E.emptyEnv
        let args = [List []]
        result <- runEval (cdr args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Cdr should fail on empty list"

    it "fails on non-list" do
        let initialEnv = E.emptyEnv
        let args = [Number 42]
        result <- runEval (cdr args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Cdr should fail on non-list"
