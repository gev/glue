module Glue.Lib.List.CdrSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Cdr (cdr)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Cdr (Test cdr function)" do
    it "returns the rest of a list" do
        let args = [List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (cdr args) []
        case result of
            Left err -> expectationFailure $ "Cdr failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 2, Integer 3]

    it "fails on empty list" do
        let args = [List []]
        result <- runEvalSimple (cdr args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Cdr should fail on empty list"

    it "fails on non-list" do
        let args = [Integer 42]
        result <- runEvalSimple (cdr args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Cdr should fail on non-list"
