module Reactor.Lib.Math.Trigonometric.AsinSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (toRealFloat)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Trigonometric.Asin qualified as Asin (asin)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Trigonometric.Asin (Test asin function)" do
    describe "Arcsine function" do
        it "returns 0 for asin(0)" do
            let args = [Number 0]
            result <- runEval (Asin.asin args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Asin failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 0

        it "returns π/2 for asin(1)" do
            let args = [Number 1]
            result <- runEval (Asin.asin args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Asin failed: " <> show err
                Right (res, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - pi / 2) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Asin.asin args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Asin.asin args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Asin.asin args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
