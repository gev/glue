module Reactor.Lib.Bool.IfSpec (spec) where

import Data.Either (isLeft)
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Bool.If (if_)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Bool.If (Test if special form)" do
    describe "Conditional execution" do
        it "executes then branch when condition is true" do
            let args = [Symbol "true", Number 42, Number 0]
            result <- runEval (if_ args) lib
            case result of
                Left err -> expectationFailure $ "If failed: " <> show err
                Right (res, _) -> res `shouldBe` Just (Number 42)

        it "executes else branch when condition is false" do
            let args = [Symbol "false", Number 42, Number 0]
            result <- runEval (if_ args) lib
            case result of
                Left err -> expectationFailure $ "If failed: " <> show err
                Right (res, _) -> res `shouldBe` Just (Number 0)

        it "fails with wrong number of arguments" do
            let args = [Symbol "true", Number 42]
            result <- runEval (if_ args) lib
            result `shouldSatisfy` isLeft
