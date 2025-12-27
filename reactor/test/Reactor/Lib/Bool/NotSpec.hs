module Reactor.Lib.Bool.NotSpec (spec) where

import Data.Either (isLeft)
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Bool.Not (not_)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Bool.Not (Test not function)" do
    describe "Logical negation" do
        it "returns false for true" do
            let args = [Symbol "true"]
            result <- runEval (not_ args) lib
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "false"

        it "returns true for false" do
            let args = [Symbol "false"]
            result <- runEval (not_ args) lib
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "true"

        it "returns false for other values" do
            let args = [Number 42]
            result <- runEval (not_ args) lib
            case result of
                Left err -> expectationFailure $ "Not failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "false"

        it "fails with wrong number of arguments" do
            let args = []
            result <- runEval (not_ args) lib
            result `shouldSatisfy` isLeft

        it "! alias works identically to not" do
            let args1 = [Symbol "false"] -- not false = true
            let args2 = [Symbol "true"] -- not true = false
            let args3 = [Number 42] -- not 42 = false (truthy)
            result1 <- runEval (not_ args1) lib
            result2 <- runEval (not_ args2) lib
            result3 <- runEval (not_ args3) lib
            case (result1, result2, result3) of
                (Right (Symbol "true", _), Right (Symbol "false", _), Right (Symbol "false", _)) -> pure ()
                _ -> expectationFailure "! alias should work like not"
