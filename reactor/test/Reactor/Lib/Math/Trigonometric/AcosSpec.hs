module Reactor.Lib.Math.Trigonometric.AcosSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import qualified Reactor.Lib.Math.Trigonometric.Acos as Acos (acos)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Trigonometric.Acos (Test acos function)" do
    describe "Arccosine function" do
        it "returns π for acos(-1)" do
            let args = [Number (fromFloatDigits (-1))]
            result <- runEval (Acos.acos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Acos failed: " <> show err
                Right (res, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat x - pi) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns π/2 for acos(0)" do
            let args = [Number 0]
            result <- runEval (Acos.acos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Acos failed: " <> show err
                Right (res, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat x - pi/2) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns 0 for acos(1)" do
            let args = [Number 1]
            result <- runEval (Acos.acos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Acos failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 0

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Acos.acos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Acos.acos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Acos.acos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
