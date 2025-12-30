module Reactor.Lib.Math.Utility.AbsSpec (spec) where

import Data.Either (isLeft)
import Reactor.Env qualified as E
import Reactor.Eval (runEvalLegacy)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import qualified Reactor.Lib.Math.Utility.Abs as Abs (abs)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Utility.Abs (Test abs function)" do
    describe "Absolute value function" do
        it "returns 5 for abs(5)" do
            let args = [Number 5]
            result <- runEvalLegacy (Abs.abs args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Abs failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 5

        it "returns 5 for abs(-5)" do
            let args = [Number (-5)]
            result <- runEvalLegacy (Abs.abs args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Abs failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 5

        it "returns 0 for abs(0)" do
            let args = [Number 0]
            result <- runEvalLegacy (Abs.abs args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Abs failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 0

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalLegacy (Abs.abs args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEvalLegacy (Abs.abs args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Abs.abs args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
