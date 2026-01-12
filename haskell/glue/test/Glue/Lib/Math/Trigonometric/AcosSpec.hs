module Glue.Lib.Math.Trigonometric.AcosSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Trigonometric.Acos qualified as Acos (acos)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Trigonometric.Acos (Test acos function)" do
    describe "Arccosine function" do
        it "returns π for acos(-1)" do
            let args = [Float (-1)]
            result <- runEvalSimple (Acos.acos args) []
            case result of
                Left err -> expectationFailure $ "Acos failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - pi) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns π/2 for acos(0)" do
            let args = [Integer 0]
            result <- runEvalSimple (Acos.acos args) []
            case result of
                Left err -> expectationFailure $ "Acos failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - pi / 2) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "returns 0 for acos(1)" do
            let args = [Integer 1]
            result <- runEvalSimple (Acos.acos args) []
            case result of
                Left err -> expectationFailure $ "Acos failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 0

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (Acos.acos args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (Acos.acos args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Acos.acos args) []
            result `shouldSatisfy` isLeft
