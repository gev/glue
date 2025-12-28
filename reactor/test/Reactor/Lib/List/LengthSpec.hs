module Reactor.Lib.List.LengthSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib.List.Length qualified as Length
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Length (Test length function)" do
    it "returns 0 for empty list" do
        let initialEnv = E.emptyEnv
        let args = [List []]
        result <- runEval (Length.length args) initialEnv
        case result of
            Left err -> expectationFailure $ "Length failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 0

    it "returns correct length for non-empty list" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, Number 2, Number 3]]
        result <- runEval (Length.length args) initialEnv
        case result of
            Left err -> expectationFailure $ "Length failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 3

    it "returns length for list with mixed types" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 42, String "hello", Number 3.14]]
        result <- runEval (Length.length args) initialEnv
        case result of
            Left err -> expectationFailure $ "Length failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Number 3

    it "fails on non-list" do
        let initialEnv = E.emptyEnv
        let args = [Number 42]
        result <- runEval (Length.length args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Length should fail on non-list"
