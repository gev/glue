module Glue.Lib.List.NthSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Nth (nth)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Nth (Test nth function)" do
    it "returns element at index 0" do
        let initialEnv = E.emptyEnv
        let args = [Integer 0, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (nth args) initialEnv
        case result of
            Left err -> expectationFailure $ "Nth failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Integer 1

    it "returns element at index 1" do
        let initialEnv = E.emptyEnv
        let args = [Integer 1, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (nth args) initialEnv
        case result of
            Left err -> expectationFailure $ "Nth failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Integer 2

    it "returns element at last index" do
        let initialEnv = E.emptyEnv
        let args = [Integer 2, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (nth args) initialEnv
        case result of
            Left err -> expectationFailure $ "Nth failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Integer 3

    it "fails on negative index" do
        let initialEnv = E.emptyEnv
        let args = [Float (-1), List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (nth args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Nth should fail on negative index"

    it "fails on index out of bounds" do
        let initialEnv = E.emptyEnv
        let args = [Integer 3, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (nth args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Nth should fail on index out of bounds"

    it "fails on non-number index" do
        let initialEnv = E.emptyEnv
        let args = [String "0", List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (nth args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Nth should fail on non-number index"

    it "fails on non-list" do
        let initialEnv = E.emptyEnv
        let args = [Integer 0, Integer 42]
        result <- runEvalSimple (nth args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Nth should fail on non-list"
