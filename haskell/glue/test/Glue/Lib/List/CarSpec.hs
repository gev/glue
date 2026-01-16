module Glue.Lib.List.CarSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Car (car)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Car (Test car function)" do
    it "returns the first element of a list" do
        let args = [List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (car args) []
        case result of
            Left err -> expectationFailure $ "Car failed: " <> show err
            Right (res, _) -> res `shouldBe` Integer 1

    it "fails on empty list" do
        let args = [List []]
        result <- runEvalSimple (car args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Car should fail on empty list"

    it "fails on non-list" do
        let args = [Integer 42]
        result <- runEvalSimple (car args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Car should fail on non-list"
