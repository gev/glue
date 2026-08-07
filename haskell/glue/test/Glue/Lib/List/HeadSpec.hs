module Glue.Lib.List.HeadSpec (spec) where

import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Head (head)
import Test.Hspec
import Prelude hiding (head)

spec :: Spec
spec = describe "Glue.Lib.List.Head (Test head function)" do
    it "returns the first element of a list" do
        let args = [List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (apply head args) []
        case result of
            Left err -> expectationFailure $ "Head failed: " <> show err
            Right (res, _) -> res `shouldBe` Integer 1

    it "fails on empty list" do
        let args = [List []]
        result <- runEvalSimple (apply head args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Head should fail on empty list"

    it "fails on non-list" do
        let args = [Integer 42]
        result <- runEvalSimple (apply head args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Head should fail on non-list"
