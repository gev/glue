module Glue.Lib.Math.Trigonometric.AtanSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Trigonometric.Atan qualified as Atan (atan)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Trigonometric.Atan (Test atan function)" do
    describe "Arctangent function" do
        it "returns 0 for atan(0)" do
            let args = [Integer 0]
            result <- runEvalSimple (Atan.atan args) []
            case result of
                Left err -> expectationFailure $ "Atan failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 0

        it "returns π/4 for atan(1)" do
            let args = [Integer 1]
            result <- runEvalSimple (Atan.atan args) []
            case result of
                Left err -> expectationFailure $ "Atan failed: " <> show err
                Right (res, _, _) -> case res of
                    Float n -> n `shouldSatisfy` (\x -> abs (x - pi / 4) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (Atan.atan args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (Atan.atan args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Atan.atan args) []
            result `shouldSatisfy` isLeft
