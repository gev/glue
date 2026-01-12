module Glue.Lib.List.FlattenSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Flatten qualified as Flatten
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Flatten (Test flatten function)" do
    it "flattens a simple nested list" do
        let initialEnv = E.emptyEnv
        let args = [List [List [Integer 1, Integer 2], List [Integer 3, Integer 4]]]
        result <- runEvalSimple (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3, Integer 4]

    it "flattens deeply nested lists" do
        let initialEnv = E.emptyEnv
        let args = [List [List [List [Integer 1], Integer 2], Integer 3]]
        result <- runEvalSimple (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3]

    it "flattens list with mixed elements" do
        let initialEnv = E.emptyEnv
        let args = [List [Integer 1, List [Integer 2, Integer 3], Integer 4]]
        result <- runEvalSimple (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3, Integer 4]

    it "flattens empty list" do
        let initialEnv = E.emptyEnv
        let args = [List []]
        result <- runEvalSimple (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "flattens list with empty sublists" do
        let initialEnv = E.emptyEnv
        let args = [List [List [], Integer 1, List []]]
        result <- runEvalSimple (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1]

    it "flattens single element list" do
        let initialEnv = E.emptyEnv
        let args = [List [Integer 42]]
        result <- runEvalSimple (Flatten.flatten args) initialEnv
        case result of
            Left err -> expectationFailure $ "Flatten failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 42]

    it "fails on non-list argument" do
        let initialEnv = E.emptyEnv
        let args = [Integer 42]
        result <- runEvalSimple (Flatten.flatten args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Flatten should fail on non-list"
