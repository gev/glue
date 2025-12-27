module Reactor.Lib.Bool.NeSpec (spec) where

import Data.Either (isLeft)
import Reactor.Eval (runEval)
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
            result <- runEval (ne args) lib
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "true"

        it "returns false for equal numbers" do
            let args = [Number 42, Number 42]
            result <- runEval (ne args) lib
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "false"

        it "returns true for unequal strings" do
            let args = [String "hello", String "world"]
            result <- runEval (ne args) lib
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "true"

        it "returns false for equal strings" do
            let args = [String "hello", String "hello"]
            result <- runEval (ne args) lib
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "false"

        it "fails with wrong number of arguments" do
            let args = [Number 42]
            result <- runEval (ne args) lib
            result `shouldSatisfy` isLeft

        it "\\= alias works identically to ne" do
            let args1 = [Number 42, Number 43] -- unequal
            let args2 = [Number 42, Number 42] -- equal
            result1 <- runEval (ne args1) lib
            result2 <- runEval (ne args2) lib
            case (result1, result2) of
                (Right (Symbol "true", _), Right (Symbol "false", _)) -> pure ()
                _ -> expectationFailure "\\= alias should work like ne"
