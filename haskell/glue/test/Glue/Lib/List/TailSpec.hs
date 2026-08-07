module Glue.Lib.List.TailSpec (spec) where

import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Tail (tail)
import Test.Hspec
import Prelude hiding (tail)

spec :: Spec
spec = describe "Glue.Lib.List.Tail (Test tail function)" do
    it "returns the rest of a list" do
        let args = [List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (apply tail args) []
        case result of
            Left err -> expectationFailure $ "Tail failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Integer 2, Integer 3]

    it "fails on empty list" do
        let args = [List []]
        result <- runEvalSimple (apply tail args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Tail should fail on empty list"

    it "fails on non-list" do
        let args = [Integer 42]
        result <- runEvalSimple (apply tail args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Tail should fail on non-list"
