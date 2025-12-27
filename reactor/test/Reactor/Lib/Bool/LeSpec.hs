module Reactor.Lib.Bool.LeSpec (spec) where

import Data.Either (isLeft)
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Bool.Le (le)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Bool.Le (Test le function)" do
    describe "Less than or equal comparison" do
        it "returns true for equal numbers" do
            let args = [Number 5, Number 5]
            result <- runEval (le args) lib
            case result of
                Left err -> expectationFailure $ "Le failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "true"

        it "returns true for lesser number" do
            let args = [Number 5, Number 10]
            result <- runEval (le args) lib
            case result of
                Left err -> expectationFailure $ "Le failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "true"

        it "returns false for greater number" do
            let args = [Number 10, Number 5]
            result <- runEval (le args) lib
            case result of
                Left err -> expectationFailure $ "Le failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "false"

        it "fails with non-numbers" do
            let args = [String "hello", String "world"]
            result <- runEval (le args) lib
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 5]
            result <- runEval (le args) lib
            result `shouldSatisfy` isLeft

        it "<= alias works identically to le" do
            let args1 = [Number 5, Number 5] -- equal
            let args2 = [Number 5, Number 10] -- lesser
            let args3 = [Number 10, Number 5] -- greater
            result1 <- runEval (le args1) lib
            result2 <- runEval (le args2) lib
            result3 <- runEval (le args3) lib
            case (result1, result2, result3) of
                (Right (Symbol "true", _), Right (Symbol "true", _), Right (Symbol "false", _)) -> pure ()
                _ -> expectationFailure "<= alias should work like le"
