module Glue.Lib.List.ReverseSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib.List.Reverse qualified as Reverse
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Reverse (Test reverse function)" do
    it "reverses a list" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Reverse.reverse args) initialEnv
        case result of
            Left err -> expectationFailure $ "Reverse failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 3, Number 2, Number 1]

    it "reverses an empty list" do
        let initialEnv = E.emptyEnv
        let args = [List []]
        result <- runEvalLegacy (Reverse.reverse args) initialEnv
        case result of
            Left err -> expectationFailure $ "Reverse failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "reverses a single element list" do
        let initialEnv = E.emptyEnv
        let args = [List [String "hello"]]
        result <- runEvalLegacy (Reverse.reverse args) initialEnv
        case result of
            Left err -> expectationFailure $ "Reverse failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [String "hello"]

    it "fails on non-list" do
        let initialEnv = E.emptyEnv
        let args = [Number 42]
        result <- runEvalLegacy (Reverse.reverse args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Reverse should fail on non-list"
