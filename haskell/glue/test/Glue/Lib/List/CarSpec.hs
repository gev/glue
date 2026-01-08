module Glue.Lib.List.CarSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib.List.Car (car)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Car (Test car function)" do
    it "returns the first element of a list" do
        let initialEnv = E.emptyEnv
        let args = [List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalLegacy (car args) initialEnv
        case result of
            Left err -> expectationFailure $ "Car failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Integer 1

    it "fails on empty list" do
        let initialEnv = E.emptyEnv
        let args = [List []]
        result <- runEvalLegacy (car args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Car should fail on empty list"

    it "fails on non-list" do
        let initialEnv = E.emptyEnv
        let args = [Integer 42]
        result <- runEvalLegacy (car args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Car should fail on non-list"
