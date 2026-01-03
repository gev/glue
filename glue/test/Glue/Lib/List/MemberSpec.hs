module Glue.Lib.List.MemberSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib.List.Member (member)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Member (Test member function)" do
    it "returns true for item in list" do
        let initialEnv = E.emptyEnv
        let args = [Number 2, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (member args) initialEnv
        case result of
            Left err -> expectationFailure $ "Member failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Symbol "true"

    it "returns false for item not in list" do
        let initialEnv = E.emptyEnv
        let args = [Number 4, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (member args) initialEnv
        case result of
            Left err -> expectationFailure $ "Member failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Symbol "false"

    it "returns true for string in list" do
        let initialEnv = E.emptyEnv
        let args = [String "hello", List [String "world", String "hello", String "test"]]
        result <- runEvalLegacy (member args) initialEnv
        case result of
            Left err -> expectationFailure $ "Member failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Symbol "true"

    it "returns false for empty list" do
        let initialEnv = E.emptyEnv
        let args = [Number 1, List []]
        result <- runEvalLegacy (member args) initialEnv
        case result of
            Left err -> expectationFailure $ "Member failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Symbol "false"

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let args = [Number 1, Number 42]
        result <- runEvalLegacy (member args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Member should fail on non-list"
