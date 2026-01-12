module Glue.Lib.List.DropSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Drop qualified as Drop
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Drop (Test drop function)" do
    it "drops first N elements from list" do
        let initialEnv = E.emptyEnv
        let args = [Integer 2, List [Integer 1, Integer 2, Integer 3, Integer 4, Integer 5]]
        result <- runEvalSimple (Drop.drop args) initialEnv
        case result of
            Left err -> expectationFailure $ "Drop failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 3, Integer 4, Integer 5]

    it "drops fewer elements when N > list length" do
        let initialEnv = E.emptyEnv
        let args = [Integer 10, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Drop.drop args) initialEnv
        case result of
            Left err -> expectationFailure $ "Drop failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "drops zero elements" do
        let initialEnv = E.emptyEnv
        let args = [Integer 0, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Drop.drop args) initialEnv
        case result of
            Left err -> expectationFailure $ "Drop failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3]

    it "drops all elements when N equals list length" do
        let initialEnv = E.emptyEnv
        let args = [Integer 3, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Drop.drop args) initialEnv
        case result of
            Left err -> expectationFailure $ "Drop failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "fails on negative count" do
        let initialEnv = E.emptyEnv
        let args = [Float (-1), List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Drop.drop args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Drop should fail on negative count"

    it "fails on non-number first argument" do
        let initialEnv = E.emptyEnv
        let args = [String "2", List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Drop.drop args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Drop should fail on non-number count"

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let args = [Integer 2, Integer 42]
        result <- runEvalSimple (Drop.drop args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Drop should fail on non-list"
