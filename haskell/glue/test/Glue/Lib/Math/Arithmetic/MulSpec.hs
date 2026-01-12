module Glue.Lib.Math.Arithmetic.MulSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Arithmetic.Mul qualified as Mul
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Arithmetic.Mul (Test mul function)" do
    describe "Mul function" do
        it "returns 6 for (* 2 3)" do
            let args = [Integer 2, Integer 3]
            result <- runEvalSimple (Mul.mul args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mul failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 6

        it "returns 7.5 for (* 2.5 3)" do
            let args = [Float 2.5, Integer 3]
            result <- runEvalSimple (Mul.mul args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mul failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 7.5

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Mul.mul args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with one argument" do
            let args = [Integer 2]
            result <- runEvalSimple (Mul.mul args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with three arguments" do
            let args = [Integer 2, Integer 3, Integer 4]
            result <- runEvalSimple (Mul.mul args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [Integer 2, String "hello"]
            result <- runEvalSimple (Mul.mul args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

    describe "Type promotion in multiplication" do
        it "Integer * Integer = Integer" do
            let args = [Integer 5, Integer 3]
            result <- runEvalSimple (Mul.mul args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mul failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 15

        it "Integer * Float = Float" do
            let args = [Integer 5, Float 3.5]
            result <- runEvalSimple (Mul.mul args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mul failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 17.5

        it "Float * Integer = Float" do
            let args = [Float 5.5, Integer 3]
            result <- runEvalSimple (Mul.mul args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mul failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 16.5

        it "Float * Float = Float" do
            let args = [Float 5.5, Float 3.5]
            result <- runEvalSimple (Mul.mul args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mul failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Float 19.25
