module Glue.Lib.Bool.IfSpec2 (spec) where

import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.If (if_)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.When (Test when special form)" do
    describe "Conditional execution" do
        it "executes body when condition is true" do
            let args = [Bool True, Integer 42]
            result <- runEvalSimple (apply if_ args) []
            case result of
                Left err -> expectationFailure $ "When failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 42

        it "does not execute body when condition is false" do
            let args = [Bool False, Integer 42]
            result <- runEvalSimple (apply if_ args) []
            case result of
                Left err -> expectationFailure $ "When failed: " <> show err
                Right (res, _) -> res `shouldBe` Void
