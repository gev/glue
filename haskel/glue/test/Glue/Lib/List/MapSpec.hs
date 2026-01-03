module Glue.Lib.List.MapSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..), Native (..))
import Glue.Lib.List.Map qualified as Map
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Map (Test map function)" do
    it "maps a function over a list of numbers" do
        let initialEnv = E.emptyEnv
        let func = Native (Func (\[Number x] -> pure $ Number (x * 2)))
        let args = [func, List [Number 1, Number 2, Number 3]]
        result <- runEvalLegacy (Map.map args) initialEnv
        case result of
            Left err -> expectationFailure $ "Map failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 2, Number 4, Number 6]

    it "maps over empty list" do
        let initialEnv = E.emptyEnv
        let func = Native (Func (\[Number x] -> pure $ Number (x + 1)))
        let args = [func, List []]
        result <- runEvalLegacy (Map.map args) initialEnv
        case result of
            Left err -> expectationFailure $ "Map failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let func = Native (Func (\[Number x] -> pure $ Number (x + 1)))
        let args = [func, Number 42]
        result <- runEvalLegacy (Map.map args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Map should fail on non-list"
