module Reactor.Lib.Math.Trigonometric.AtanSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (toRealFloat)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import qualified Reactor.Lib.Math.Trigonometric.Atan as Atan (atan)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Trigonometric.Atan (Test atan function)" do
    describe "Arctangent function" do
        it "returns 0 for atan(0)" do
            let args = [Number 0]
            result <- runEval (Atan.atan args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Atan failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 0

        it "returns π/4 for atan(1)" do
            let args = [Number 1]
            result <- runEval (Atan.atan args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Atan failed: " <> show err
                Right (res, _, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat @Double x - pi/4) < 1e-10)
                    _ -> expectationFailure "Expected Number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Atan.atan args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Atan.atan args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Atan.atan args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
