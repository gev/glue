module Reactor.Lib.Math.Arithmetic.SubSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Arithmetic.Sub qualified as Sub
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Arithmetic.Sub (Test sub function)" do
    describe "Sub function" do
        it "returns -5 for (- 5)" do
            let args = [Number 5]
            result <- runEval (Sub.sub args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sub failed: " <> show err
                Right (res, _) -> res `shouldBe` Number (-5)

        it "returns -3 for (- 1 4)" do
            let args = [Number 1, Number 4]
            result <- runEval (Sub.sub args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sub failed: " <> show err
                Right (res, _) -> res `shouldBe` Number (-3)

        it "returns 2 for (- 10 4 4)" do
            let args = [Number 10, Number 4, Number 4]
            result <- runEval (Sub.sub args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sub failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 2

        it "returns 2.5 for (- 5.5 3)" do
            let args = [Number (fromFloatDigits @Double 5.5), Number 3]
            result <- runEval (Sub.sub args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sub failed: " <> show err
                Right (res, _) -> res `shouldBe` Number (fromFloatDigits @Double 2.5)

        it "fails with no arguments" do
            let args = []
            result <- runEval (Sub.sub args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [Number 5, String "hello"]
            result <- runEval (Sub.sub args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
