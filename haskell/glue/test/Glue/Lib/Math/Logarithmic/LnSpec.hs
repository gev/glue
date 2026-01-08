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
spec = describe "Glue.Lib.Math.Logarithmic.Ln (Test ln function)" do
    describe "Natural logarithm function" do
        it "returns ln(1) = 0" do
            let args = [Integer 1]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ln failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 0

        it "returns ln(e) = 1" do
            let args = [Float (exp 1)]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ln failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 1) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns ln(e^2) = 2" do
            let args = [Float (exp 2)]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ln failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - 2) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "fails with zero" do
            let args = [Integer 0]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with negative numbers" do
            let args = [Float (-1)]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Ln.ln args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
