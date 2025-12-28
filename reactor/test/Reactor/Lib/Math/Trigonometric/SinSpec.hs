module Reactor.Lib.Math.Trigonometric.SinSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Trigonometric.Sin qualified as Sin (sin)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Trigonometric.Sin (Test sin function)" do
    describe "Sine function" do
        it "returns 0 for sin(0)" do
            let args = [Number 0]
            result <- runEval (Sin.sin args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sin failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 0

        it "returns 1 for sin(π/2)" do
            let args = [Number (fromFloatDigits @Double (pi / 2))]
            result <- runEval (Sin.sin args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sin failed: " <> show err
                Right (res, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - 1) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns 0 for sin(π)" do
            let args = [Number (fromFloatDigits @Double pi)]
            result <- runEval (Sin.sin args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sin failed: " <> show err
                Right (res, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Sin.sin args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Sin.sin args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Sin.sin args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

-- QuickCheck properties removed for now - need proper IO testing setup
