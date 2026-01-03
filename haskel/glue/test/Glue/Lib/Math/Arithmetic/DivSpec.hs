module Glue.Lib.Math.Arithmetic.DivSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Arithmetic.Div qualified as Div
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Arithmetic.Div (Test div function)" do
    describe "Div function" do
        it "returns 0.5 for (/ 2)" do
            let args = [Number 2]
            result <- runEvalLegacy (Div.div args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number (fromFloatDigits @Double 0.5)

        it "returns 2 for (/ 10 5)" do
            let args = [Number 10, Number 5]
            result <- runEvalLegacy (Div.div args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 2

        it "returns 2.5 for (/ 10 2 2)" do
            let args = [Number 10, Number 2, Number 2]
            result <- runEvalLegacy (Div.div args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number (fromFloatDigits @Double 2.5)

        it "returns 2.5 for (/ 10 4)" do
            let args = [Number 10, Number 4]
            result <- runEvalLegacy (Div.div args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Div failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number (fromFloatDigits @Double 2.5)

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Div.div args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with division by zero" do
            let args = [Number 10, Number 0]
            result <- runEvalLegacy (Div.div args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [Number 10, String "hello"]
            result <- runEvalLegacy (Div.div args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
