module Reactor.Lib.Math.Utility.ExpSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (toRealFloat)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import qualified Reactor.Lib.Math.Utility.Exp as Exp
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Utility.Exp (Test exp function)" do
    describe "Exponential function" do
        it "returns e^0 = 1" do
            let args = [Number 0]
            result <- runEval (Exp.exp args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Exp failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 1

        it "returns e^1 = e" do
            let args = [Number 1]
            result <- runEval (Exp.exp args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Exp failed: " <> show err
                Right (res, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat x - exp 1) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "returns e^2" do
            let args = [Number 2]
            result <- runEval (Exp.exp args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Exp failed: " <> show err
                Right (res, _) -> case res of
                    Number n -> n `shouldSatisfy` (\x -> abs (toRealFloat x - exp 2) < 1e-10)
                    _ -> expectationFailure "Expected a number"

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Exp.exp args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Exp.exp args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Exp.exp args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
