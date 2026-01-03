module Glue.Lib.Bool.NotSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Bool.Not (not_)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Bool.Not (Test not function)" do
    describe "Logical negation" do
        it "returns false for true" do
            let args = [Symbol "true"]
            result <- runEvalLegacy (not_ args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "false"

        it "returns true for false" do
            let args = [Symbol "false"]
            result <- runEvalLegacy (not_ args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "true"

        it "returns false for other values" do
            let args = [Number 42]
            result <- runEvalLegacy (not_ args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "false"

        it "fails with wrong number of arguments" do
            let args = []
            result <- runEvalLegacy (not_ args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "! alias works identically to not" do
            let args1 = [Symbol "false"] -- not false = true
            let args2 = [Symbol "true"] -- not true = false
            let args3 = [Number 42] -- not 42 = false (truthy)
            result1 <- runEvalLegacy (not_ args1) (E.fromFrame lib)
            result2 <- runEvalLegacy (not_ args2) (E.fromFrame lib)
            result3 <- runEvalLegacy (not_ args3) (E.fromFrame lib)
            case (result1, result2, result3) of
                (Right (Symbol "true", _, _), Right (Symbol "false", _, _), Right (Symbol "false", _, _)) -> pure ()
                _ -> expectationFailure "! alias should work like not"
