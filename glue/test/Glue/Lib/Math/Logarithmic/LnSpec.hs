module Glue.Lib.Math.Logarithmic.LnSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Logarithmic.Ln qualified as Ln
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Logarithmic.Ln (Test ln function)" do
    describe "Natural logarithm function" do
        it "returns ln(1) = 0" do
            let args = [Number 1]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ln failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 0

        it "returns ln(e) = 1" do
            let args = [Number (fromFloatDigits @Double (exp 1))]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ln failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - 1) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns ln(e^2) = 2" do
            let args = [Number (fromFloatDigits @Double (exp 2))]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ln failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - 2) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "fails with zero" do
            let args = [Number 0]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with negative numbers" do
            let args = [Number (-1)]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
