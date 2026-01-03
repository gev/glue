module Glue.Lib.Math.Trigonometric.AcosSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Trigonometric.Acos qualified as Acos (acos)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Trigonometric.Acos (Test acos function)" do
    describe "Arccosine function" do
        it "returns π for acos(-1)" do
            let args = [Number (fromFloatDigits @Double (-1))]
            result <- runEvalLegacy (Acos.acos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Acos failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - pi) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns π/2 for acos(0)" do
            let args = [Number 0]
            result <- runEvalLegacy (Acos.acos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Acos failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - pi / 2) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns 0 for acos(1)" do
            let args = [Number 1]
            result <- runEvalLegacy (Acos.acos args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Acos failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 0

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalLegacy (Acos.acos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEvalLegacy (Acos.acos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Acos.acos args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
