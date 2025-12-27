module Reactor.Lib.Math.Arithmetic.MulSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Arithmetic.Mul qualified as Mul
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Arithmetic.Mul (Test mul function)" do
    describe "Mul function" do
        it "returns 6 for (* 2 3)" do
            let args = [Number 2, Number 3]
            result <- runEval (Mul.mul args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mul failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 6

        it "returns 24 for (* 2 3 4)" do
            let args = [Number 2, Number 3, Number 4]
            result <- runEval (Mul.mul args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mul failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 24

        it "returns 7.5 for (* 2.5 3)" do
            let args = [Number (fromFloatDigits @Double 2.5), Number 3]
            result <- runEval (Mul.mul args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mul failed: " <> show err
                Right (res, _) -> res `shouldBe` Number (fromFloatDigits @Double 7.5)

        it "fails with no arguments" do
            let args = []
            result <- runEval (Mul.mul args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [Number 2, String "hello"]
            result <- runEval (Mul.mul args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
