module Glue.Lib.List.ConsSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Cons (cons)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Cons (Test cons function)" do
    it "constructs a list by prepending an element" do
        let initialEnv = E.emptyEnv
        let args = [Integer 1, List [Integer 2, Integer 3]]
        result <- runEvalSimple (cons args) initialEnv
        case result of
            Left err -> expectationFailure $ "Cons failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Integer 1, Integer 2, Integer 3]

    it "constructs a list with empty tail" do
        let initialEnv = E.emptyEnv
        let args = [String "hello", List []]
        result <- runEvalSimple (cons args) initialEnv
        case result of
            Left err -> expectationFailure $ "Cons failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [String "hello"]

    it "fails on non-list tail" do
        let initialEnv = E.emptyEnv
        let args = [Integer 1, Integer 2]
        result <- runEvalSimple (cons args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Cons should fail on non-list tail"
