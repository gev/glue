module Reactor.Lib.List.ConsSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib.List.Cons (cons)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Cons (Test cons function)" do
    it "constructs a list by prepending an element" do
        let initialEnv = E.emptyEnv
        let args = [Number 1, List [Number 2, Number 3]]
        result <- runEval (cons args) initialEnv
        case result of
            Left err -> expectationFailure $ "Cons failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3]

    it "constructs a list with empty tail" do
        let initialEnv = E.emptyEnv
        let args = [String "hello", List []]
        result <- runEval (cons args) initialEnv
        case result of
            Left err -> expectationFailure $ "Cons failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [String "hello"]

    it "fails on non-list tail" do
        let initialEnv = E.emptyEnv
        let args = [Number 1, Number 2]
        result <- runEval (cons args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Cons should fail on non-list tail"
