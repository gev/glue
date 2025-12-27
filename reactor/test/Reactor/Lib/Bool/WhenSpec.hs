module Reactor.Lib.Bool.WhenSpec (spec) where

import Data.Either (isLeft)
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Bool.When (when_)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Bool.When (Test when special form)" do
    describe "Conditional execution" do
        it "executes body when condition is true" do
            let args = [Symbol "true", Number 42]
            result <- runEval (when_ args) lib
            case result of
                Left err -> expectationFailure $ "When failed: " <> show err
                Right (res, _) -> res `shouldBe` Just (Number 42)

        it "does not execute body when condition is false" do
            let args = [Symbol "false", Number 42]
            result <- runEval (when_ args) lib
            case result of
                Left err -> expectationFailure $ "When failed: " <> show err
                Right (res, _) -> res `shouldBe` Nothing

        it "executes multiple body expressions and returns last" do
            let args = [Symbol "true", Number 1, Number 2, Number 3]
            result <- runEval (when_ args) lib
            case result of
                Left err -> expectationFailure $ "When failed: " <> show err
                Right (res, _) -> res `shouldBe` Just (Number 3)

        it "fails with wrong number of arguments" do
            let args = [] -- No condition
            result <- runEval (when_ args) lib
            result `shouldSatisfy` isLeft
