module Reactor.Lib.Math.Logarithmic.LgSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (toRealFloat)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Logarithmic.Lg qualified as Lg
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Logarithmic.Lg (Test lg function)" do
    describe "Common logarithm function (base 10)" do
        it "returns lg(1) = 0" do
            let args = [Number 1]
            result <- runEval (Lg.lg args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lg failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 0

        it "returns lg(10) = 1" do
            let args = [Number 10]
            result <- runEval (Lg.lg args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lg failed: " <> show err
                Right (res, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - 1) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns lg(100) = 2" do
            let args = [Number 100]
            result <- runEval (Lg.lg args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lg failed: " <> show err
                Right (res, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - 2) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "fails with zero" do
            let args = [Number 0]
            result <- runEval (Lg.lg args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with negative numbers" do
            let args = [Number (-1)]
            result <- runEval (Lg.lg args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Lg.lg args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Lg.lg args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Lg.lg args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
