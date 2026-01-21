module Glue.Lib.Bool.IfSpec (spec) where

import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.If (if_)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.If (Test if special form)" do
    describe "Conditional execution" do
        it "executes then branch when condition is true" do
            let args = [Bool True, Integer 42, Integer 0]
            result <- runEvalSimple (apply if_ args) []
            case result of
                Left err -> expectationFailure $ "If failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 42

        it "executes else branch when condition is false" do
            let args = [Bool False, Integer 42, Integer 0]
            result <- runEvalSimple (apply if_ args) []
            case result of
                Left err -> expectationFailure $ "If failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 0
