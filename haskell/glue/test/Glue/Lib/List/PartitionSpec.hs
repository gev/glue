module Glue.Lib.List.PartitionSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..), Native (..))
import Glue.Lib.List.Partition qualified as Partition
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Partition (Test partition function)" do
    it "partitions list into matching and non-matching elements" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Integer x] -> pure . Bool $ x > 3))
        let args = [pred, List [Integer 1, Integer 2, Integer 3, Integer 4, Integer 5]]
        result <- runEvalSimple (Partition.partition args) initialEnv
        case result of
            Left err -> expectationFailure $ "Partition failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Integer 4, Integer 5], List [Integer 1, Integer 2, Integer 3]]

    it "partitions list with all elements matching" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Integer x] -> pure . Bool $ x > 0))
        let args = [pred, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Partition.partition args) initialEnv
        case result of
            Left err -> expectationFailure $ "Partition failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Integer 1, Integer 2, Integer 3], List []]

    it "partitions list with no elements matching" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Integer x] -> pure . Bool $ x > 10))
        let args = [pred, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Partition.partition args) initialEnv
        case result of
            Left err -> expectationFailure $ "Partition failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [], List [Integer 1, Integer 2, Integer 3]]

    it "partitions empty list" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Integer x] -> pure $ Bool True))
        let args = [pred, List []]
        result <- runEvalSimple (Partition.partition args) initialEnv
        case result of
            Left err -> expectationFailure $ "Partition failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [], List []]

    it "partitions list with mixed matching" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Integer x] -> pure . Bool $ x `mod` 2 == 0))
        let args = [pred, List [Integer 1, Integer 2, Integer 3, Integer 4, Integer 5]]
        result <- runEvalSimple (Partition.partition args) initialEnv
        case result of
            Left err -> expectationFailure $ "Partition failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Integer 2, Integer 4], List [Integer 1, Integer 3, Integer 5]]

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Integer x] -> pure $ Bool True))
        let args = [pred, Integer 42]
        result <- runEvalSimple (Partition.partition args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Partition should fail on non-list"
