module Reactor.Lib.Math.Utility.CeilSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Utility.Ceil qualified as Ceil
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Utility.Ceil (Test ceil function)" do
    describe "Ceil function" do
        it "returns 4 for ceil(3.1)" do
            let args = [Number (fromFloatDigits 3.1)]
            result <- runEval (Ceil.ceil args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 4

        it "returns 3 for ceil(2.9)" do
            let args = [Number (fromFloatDigits 2.9)]
            result <- runEval (Ceil.ceil args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 3

        it "returns -3 for ceil(-3.1)" do
            let args = [Number (fromFloatDigits (-3.1))]
            result <- runEval (Ceil.ceil args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _) -> res `shouldBe` Number (-3)

        it "returns 5 for ceil(5.0)" do
            let args = [Number 5]
            result <- runEval (Ceil.ceil args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 5

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Ceil.ceil args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Ceil.ceil args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Ceil.ceil args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
