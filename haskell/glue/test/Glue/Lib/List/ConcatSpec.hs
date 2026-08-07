module Glue.Lib.List.ConcatSpec (spec) where

import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Concat (concat)
import Test.Hspec
import Prelude hiding (concat)

spec :: Spec
spec = describe "Glue.Lib.List.Concat (Test concat function)" do
    it "concats two lists" do
        let initialEnv = []
        let args = [List [Integer 1, Integer 2], List [Integer 3, Integer 4]]
        result <- runEvalSimple (apply concat args) initialEnv
        case result of
            Left err -> expectationFailure $ "Concat failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3, Integer 4]

    it "concats empty list to non-empty list" do
        let initialEnv = []
        let args = [List [Integer 1, Integer 2], List []]
        result <- runEvalSimple (apply concat args) initialEnv
        case result of
            Left err -> expectationFailure $ "Concat failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Integer 1, Integer 2]

    it "concats non-empty list to empty list" do
        let initialEnv = []
        let args = [List [], List [Integer 3, Integer 4]]
        result <- runEvalSimple (apply concat args) initialEnv
        case result of
            Left err -> expectationFailure $ "Concat failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Integer 3, Integer 4]

    it "concats two empty lists" do
        let initialEnv = []
        let args = [List [], List []]
        result <- runEvalSimple (apply concat args) initialEnv
        case result of
            Left err -> expectationFailure $ "Concat failed: " <> show err
            Right (res, _) -> res `shouldBe` List []

    it "fails on non-list first argument" do
        let initialEnv = []
        let args = [Integer 42, List [Integer 1]]
        result <- runEvalSimple (apply concat args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Concat should fail on non-list first argument"
