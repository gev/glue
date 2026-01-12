module Glue.Lib.List.MemberSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Member (member)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Member (Test member function)" do
    it "returns true for item in list" do
        let initialEnv = E.emptyEnv
        let args = [Integer 2, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (member args) initialEnv
        case result of
            Left err -> expectationFailure $ "Member failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Bool True

    it "returns false for item not in list" do
        let initialEnv = E.emptyEnv
        let args = [Integer 4, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (member args) initialEnv
        case result of
            Left err -> expectationFailure $ "Member failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Bool False

    it "returns true for string in list" do
        let initialEnv = E.emptyEnv
        let args = [String "hello", List [String "world", String "hello", String "test"]]
        result <- runEvalSimple (member args) initialEnv
        case result of
            Left err -> expectationFailure $ "Member failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Bool True

    it "returns false for empty list" do
        let initialEnv = E.emptyEnv
        let args = [Integer 1, List []]
        result <- runEvalSimple (member args) initialEnv
        case result of
            Left err -> expectationFailure $ "Member failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Bool False

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let args = [Integer 1, Integer 42]
        result <- runEvalSimple (member args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Member should fail on non-list"
