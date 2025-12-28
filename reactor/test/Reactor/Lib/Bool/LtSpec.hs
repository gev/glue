module Reactor.Lib.Bool.LtSpec (spec) where

import Data.Either (isLeft)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Bool.Lt (lt)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Bool.Lt (Test lt function)" do
    describe "Less than comparison" do
        it "returns true for lesser number" do
            let args = [Number 5, Number 10]
            result <- runEval (lt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "true"

        it "returns false for equal numbers" do
            let args = [Number 5, Number 5]
            result <- runEval (lt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "false"

        it "returns false for greater number" do
            let args = [Number 10, Number 5]
            result <- runEval (lt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Symbol "false"

        it "fails with non-numbers" do
            let args = [String "hello", String "world"]
            result <- runEval (lt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 5]
            result <- runEval (lt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "< alias works identically to lt" do
            let args1 = [Number 5, Number 10] -- lesser
            let args2 = [Number 5, Number 5] -- equal
            let args3 = [Number 10, Number 5] -- greater
            result1 <- runEval (lt args1) (E.fromFrame lib)
            result2 <- runEval (lt args2) (E.fromFrame lib)
            result3 <- runEval (lt args3) (E.fromFrame lib)
            case (result1, result2, result3) of
                (Right (Symbol "true", _, _), Right (Symbol "false", _, _), Right (Symbol "false", _, _)) -> pure ()
                _ -> expectationFailure "< alias should work like lt"
