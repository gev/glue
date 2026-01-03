module Glue.Lib.List.AppendSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib.List.Append (append)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Append (Test append function)" do
    it "appends two lists" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, Number 2], List [Number 3, Number 4]]
        result <- runEvalLegacy (append args) initialEnv
        case result of
            Left err -> expectationFailure $ "Append failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2, Number 3, Number 4]

    it "appends empty list to non-empty list" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, Number 2], List []]
        result <- runEvalLegacy (append args) initialEnv
        case result of
            Left err -> expectationFailure $ "Append failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2]

    it "appends non-empty list to empty list" do
        let initialEnv = E.emptyEnv
        let args = [List [], List [Number 3, Number 4]]
        result <- runEvalLegacy (append args) initialEnv
        case result of
            Left err -> expectationFailure $ "Append failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 3, Number 4]

    it "appends two empty lists" do
        let initialEnv = E.emptyEnv
        let args = [List [], List []]
        result <- runEvalLegacy (append args) initialEnv
        case result of
            Left err -> expectationFailure $ "Append failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "fails on non-list first argument" do
        let initialEnv = E.emptyEnv
        let args = [Number 42, List [Number 1]]
        result <- runEvalLegacy (append args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Append should fail on non-list first argument"

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1], Number 42]
        result <- runEvalLegacy (append args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Append should fail on non-list second argument"
