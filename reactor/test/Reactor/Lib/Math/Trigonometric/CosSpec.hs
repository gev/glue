module Reactor.Lib.Math.Trigonometric.CosSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Trigonometric.Cos qualified as Cos (cos)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Trigonometric.Cos (Test cos function)" do
    describe "Cosine function" do
        it "returns 1 for cos(0)" do
            let args = [Number 0]
            result <- runEval (Cos.cos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 1

        it "returns 0 for cos(π/2)" do
            let args = [Number (fromFloatDigits @Double (pi / 2))]
            result <- runEval (Cos.cos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns -1 for cos(π)" do
            let args = [Number (fromFloatDigits @Double pi)]
            result <- runEval (Cos.cos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Cos failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - (-1)) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Cos.cos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Cos.cos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Cos.cos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
