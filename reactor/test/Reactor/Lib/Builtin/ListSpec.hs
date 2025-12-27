module Reactor.Lib.Builtin.ListSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib.Builtin.List (list)
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
                Right (res, _) -> res `shouldBe` List [Number 1, Number 2, Number 3]

        it "creates an empty list" do
            let initialEnv = E.emptyEnv
            let args = []
            result <- runEval (list args) initialEnv
            case result of
                Left err -> expectationFailure $ "List failed: " <> show err
                Right (res, _) -> res `shouldBe` List []

        it "creates a list with mixed types" do
            let initialEnv = E.emptyEnv
            let args = [Number 42, String "hello", Symbol "x"]
            result <- runEval (list args) initialEnv
            case result of
                Left err -> expectationFailure $ "List failed: " <> show err
                Right (res, _) -> res `shouldBe` List [Number 42, String "hello", Symbol "x"]
