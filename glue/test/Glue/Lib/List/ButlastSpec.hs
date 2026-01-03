module Glue.Lib.List.ButlastSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib.List.Butlast (butlast)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Butlast (Test butlast function)" do
    it "returns all elements except the last one" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (butlast args) initialEnv
        case result of
            Left err -> expectationFailure $ "Butlast failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 2]

    it "returns empty list for single-element list" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 42]]
        result <- runEvalLegacy (butlast args) initialEnv
        case result of
            Left err -> expectationFailure $ "Butlast failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "returns string elements except last" do
        let initialEnv = E.emptyEnv
        let args = [List [String "hello", String "world", String "test"]]
        result <- runEvalLegacy (butlast args) initialEnv
        case result of
            Left err -> expectationFailure $ "Butlast failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [String "hello", String "world"]

    it "fails on empty list" do
        let initialEnv = E.emptyEnv
        let args = [List []]
        result <- runEvalLegacy (butlast args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Butlast should fail on empty list"

    it "fails on non-list argument" do
        let initialEnv = E.emptyEnv
        let args = [Number 42]
        result <- runEvalLegacy (butlast args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Butlast should fail on non-list"
