module Glue.Lib.List.PrependSpec (spec) where

import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Prepend (prepend)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Prepend (Test prepend function)" do
    it "prependtructs a list by prepending an element" do
        let args = [Integer 1, List [Integer 2, Integer 3]]
        result <- runEvalSimple (apply prepend args) []
        case result of
            Left err -> expectationFailure $ "Prepend failed: " <> show err
            Right (res, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3]

    it "prependtructs a list with empty tail" do
        let args = [String "hello", List []]
        result <- runEvalSimple (apply prepend args) []
        case result of
            Left err -> expectationFailure $ "Prepend failed: " <> show err
            Right (res, _) -> res `shouldBe` List [String "hello"]

    it "fails on non-list tail" do
        let args = [Integer 1, Integer 2]
        result <- runEvalSimple (apply prepend args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Prepend should fail on non-list tail"
