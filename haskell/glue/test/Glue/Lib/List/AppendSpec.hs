module Glue.Lib.List.AppendSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Append (append)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Append (Test append function)" do
    it "appends two lists" do
        let initialEnv = []
        let args = [List [Integer 1, Integer 2], List [Integer 3, Integer 4]]
        result <- runEvalSimple (append args) initialEnv
        case result of
            Left err -> expectationFailure $ "Append failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3, Integer 4]

    it "appends empty list to non-empty list" do
        let initialEnv = []
        let args = [List [Integer 1, Integer 2], List []]
        result <- runEvalSimple (append args) initialEnv
        case result of
            Left err -> expectationFailure $ "Append failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Integer 1, Integer 2]

    it "appends non-empty list to empty list" do
        let initialEnv = []
        let args = [List [], List [Integer 3, Integer 4]]
        result <- runEvalSimple (append args) initialEnv
        case result of
            Left err -> expectationFailure $ "Append failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Integer 3, Integer 4]

    it "appends two empty lists" do
        let initialEnv = []
        let args = [List [], List []]
        result <- runEvalSimple (append args) initialEnv
        case result of
            Left err -> expectationFailure $ "Append failed: " <> show err
            Right (res, _) -> res `shouldBe` List []

    it "fails on non-list first argument" do
        let initialEnv = []
        let args = [Integer 42, List [Integer 1]]
        result <- runEvalSimple (append args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Append should fail on non-list first argument"

    it "fails on non-list second argument" do
        let initialEnv = []
        let args = [List [Integer 1], Integer 42]
        result <- runEvalSimple (append args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Append should fail on non-list second argument"
