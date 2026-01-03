module Glue.Lib.Math.Trigonometric.TanSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Trigonometric.Tan qualified as Tan (tan)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Trigonometric.Tan (Test tan function)" do
    describe "Tangent function" do
        it "returns 0 for tan(0)" do
            let args = [Number 0]
            result <- runEvalLegacy (Tan.tan args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Tan failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 0

        it "returns 1 for tan(π/4)" do
            let args = [Number (fromFloatDigits @Double (pi / 4))]
            result <- runEvalLegacy (Tan.tan args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Tan failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - 1) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalLegacy (Tan.tan args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEvalLegacy (Tan.tan args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Tan.tan args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
