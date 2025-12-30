module Reactor.Lib.Math.Utility.CeilSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits)
import Reactor.Env qualified as E
import Reactor.Eval (runEvalLegacy)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Utility.Ceil qualified as Ceil
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Utility.Ceil (Test ceil function)" do
    describe "Ceil function" do
        it "returns 4 for ceil(3.1)" do
            let args = [Number (fromFloatDigits @Double 3.1)]
            result <- runEvalLegacy (Ceil.ceil args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 4

        it "returns 3 for ceil(2.9)" do
            let args = [Number (fromFloatDigits @Double 2.9)]
            result <- runEvalLegacy (Ceil.ceil args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 3

        it "returns -3 for ceil(-3.1)" do
            let args = [Number (fromFloatDigits @Double (-3.1))]
            result <- runEvalLegacy (Ceil.ceil args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number (-3)

        it "returns 5 for ceil(5.0)" do
            let args = [Number 5]
            result <- runEvalLegacy (Ceil.ceil args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 5

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalLegacy (Ceil.ceil args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEvalLegacy (Ceil.ceil args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Ceil.ceil args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
