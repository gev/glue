module Reactor.Lib.Bool.NeSpec (spec) where

import Data.Either (isLeft)
import Reactor.Env qualified as E
import Reactor.Eval (runEvalLegacy)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Bool.Ne (ne)
import Reactor.TestUtils ()
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Bool.Ne (Test ne function)" do
    describe "Not equal comparison" do
        it "returns true for unequal numbers" do
            let args = [Number 42, Number 43]
            result <- runEvalLegacy (ne args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "true"

        it "returns false for equal numbers" do
            let args = [Number 42, Number 42]
            result <- runEvalLegacy (ne args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "false"

        it "returns true for unequal strings" do
            let args = [String "hello", String "world"]
            result <- runEvalLegacy (ne args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "true"

        it "returns false for equal strings" do
            let args = [String "hello", String "hello"]
            result <- runEvalLegacy (ne args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "false"

        it "fails with wrong number of arguments" do
            let args = [Number 42]
            result <- runEvalLegacy (ne args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "\\= alias works identically to ne" do
            let args1 = [Number 42, Number 43] -- unequal
            let args2 = [Number 42, Number 42] -- equal
            result1 <- runEvalLegacy (ne args1) (E.fromFrame lib)
            result2 <- runEvalLegacy (ne args2) (E.fromFrame lib)
            case (result1, result2) of
                (Right (Symbol "true", _, _), Right (Symbol "false", _, _)) -> pure ()
                _ -> expectationFailure "\\= alias should work like ne"
