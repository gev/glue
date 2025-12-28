module Reactor.Lib.Math.Logarithmic.LogSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Logarithmic.Log qualified as Log
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Logarithmic.Log (Test log function)" do
    describe "Logarithm function with base" do
        it "returns log(1, e) = 0" do
            let args = [Number 1, Number (fromFloatDigits @Double (exp 1))]
            result <- runEval (Log.log args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Log failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 0

        it "returns log(e, e) = 1" do
            let e = fromFloatDigits @Double (exp 1)
            let args = [Number e, Number e]
            result <- runEval (Log.log args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Log failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - 1) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns log(e^2, e) = 2" do
            let e = fromFloatDigits @Double (exp 1)
            let e2 = fromFloatDigits @Double (exp 2)
            let args = [Number e2, Number e]
            result <- runEval (Log.log args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Log failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - 2) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns log(100, 10) = 2" do
            let args = [Number 100, Number 10]
            result <- runEval (Log.log args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Log failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 2

        it "returns log(8, 2) = 3" do
            let args = [Number 8, Number 2]
            result <- runEval (Log.log args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Log failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 3

        it "fails with zero value" do
            let args = [Number 0, Number 10]
            result <- runEval (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with negative value" do
            let args = [Number (-1), Number 10]
            result <- runEval (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with zero base" do
            let args = [Number 10, Number 0]
            result <- runEval (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with negative base" do
            let args = [Number 10, Number (-1)]
            result <- runEval (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with base 1" do
            let args = [Number 10, Number 1]
            result <- runEval (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers (value)" do
            let args = [String "hello", Number 10]
            result <- runEval (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers (base)" do
            let args = [Number 10, String "hello"]
            result <- runEval (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (one)" do
            let args = [Number 1]
            result <- runEval (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (three)" do
            let args = [Number 1, Number 2, Number 3]
            result <- runEval (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Log.log args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
