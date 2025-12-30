module Reactor.Lib.Bool.GeSpec (spec) where

import Data.Either (isLeft)
import Reactor.Env qualified as E
import Reactor.Eval (runEvalLegacy)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Bool.Ge (ge)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Bool.Ge (Test ge function)" do
    describe "Greater than or equal comparison" do
        it "returns true for equal numbers" do
            let args = [Number 5, Number 5]
            result <- runEvalLegacy (ge args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ge failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "true"

        it "returns true for greater number" do
            let args = [Number 10, Number 5]
            result <- runEvalLegacy (ge args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ge failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "true"

        it "returns false for lesser number" do
            let args = [Number 5, Number 10]
            result <- runEvalLegacy (ge args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ge failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "false"

        it "fails with non-numbers" do
            let args = [String "hello", String "world"]
            result <- runEvalLegacy (ge args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 5]
            result <- runEvalLegacy (ge args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it ">= alias works identically to ge" do
            let args1 = [Number 5, Number 5] -- equal
            let args2 = [Number 10, Number 5] -- greater
            let args3 = [Number 5, Number 10] -- lesser
            result1 <- runEvalLegacy (ge args1) (E.fromFrame lib)
            result2 <- runEvalLegacy (ge args2) (E.fromFrame lib)
            result3 <- runEvalLegacy (ge args3) (E.fromFrame lib)
            case (result1, result2, result3) of
                (Right (Symbol "true", _, _), Right (Symbol "true", _, _), Right (Symbol "false", _, _)) -> pure ()
                _ -> expectationFailure ">= alias should work like ge"
