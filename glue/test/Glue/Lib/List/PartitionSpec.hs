module Glue.Lib.List.PartitionSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..), Native (..))
import Glue.Lib.List.Partition qualified as Partition
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Partition (Test partition function)" do
    it "partitions list into matching and non-matching elements" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ if x > 3 then Symbol "true" else Symbol "false"))
        let args = [pred, List [Number 1, Number 2, Number 3, Number 4, Number 5]]
        result <- runEvalLegacy (Partition.partition args) initialEnv
        case result of
            Left err -> expectationFailure $ "Partition failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Number 4, Number 5], List [Number 1, Number 2, Number 3]]

    it "partitions list with all elements matching" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ if x > 0 then Symbol "true" else Symbol "false"))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Partition.partition args) initialEnv
        case result of
            Left err -> expectationFailure $ "Partition failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Number 1, Number 2, Number 3], List []]

    it "partitions list with no elements matching" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ if x > 10 then Symbol "true" else Symbol "false"))
        let args = [pred, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Partition.partition args) initialEnv
        case result of
            Left err -> expectationFailure $ "Partition failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [], List [Number 1, Number 2, Number 3]]

    it "partitions empty list" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Symbol "true"))
        let args = [pred, List []]
        result <- runEvalLegacy (Partition.partition args) initialEnv
        case result of
            Left err -> expectationFailure $ "Partition failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [], List []]

    it "partitions list with mixed matching" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ if floor x `mod` 2 == 0 then Symbol "true" else Symbol "false"))
        let args = [pred, List [Number 1, Number 2, Number 3, Number 4, Number 5]]
        result <- runEvalLegacy (Partition.partition args) initialEnv
        case result of
            Left err -> expectationFailure $ "Partition failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Number 2, Number 4], List [Number 1, Number 3, Number 5]]

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let pred = Native (Func (\[Number x] -> pure $ Symbol "true"))
        let args = [pred, Number 42]
        result <- runEvalLegacy (Partition.partition args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Partition should fail on non-list"
