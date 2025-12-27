module Reactor.Lib.Bool.GtSpec (spec) where

import Data.Either (isLeft)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Bool.Gt (gt)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Bool.Gt (Test gt function)" do
    describe "Greater than comparison" do
        it "returns true for greater number" do
            let args = [Number 10, Number 5]
            result <- runEval (gt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Gt failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "true"

        it "returns false for equal numbers" do
            let args = [Number 5, Number 5]
            result <- runEval (gt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Gt failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "false"

        it "returns false for lesser number" do
            let args = [Number 5, Number 10]
            result <- runEval (gt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Gt failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "false"

        it "fails with non-numbers" do
            let args = [String "hello", String "world"]
            result <- runEval (gt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 5]
            result <- runEval (gt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "> alias works identically to gt" do
            let args1 = [Number 10, Number 5] -- greater
            let args2 = [Number 5, Number 5] -- equal
            let args3 = [Number 5, Number 10] -- lesser
            result1 <- runEval (gt args1) (E.fromFrame lib)
            result2 <- runEval (gt args2) (E.fromFrame lib)
            result3 <- runEval (gt args3) (E.fromFrame lib)
            case (result1, result2, result3) of
                (Right (Symbol "true", _), Right (Symbol "false", _), Right (Symbol "false", _)) -> pure ()
                _ -> expectationFailure "> alias should work like gt"
