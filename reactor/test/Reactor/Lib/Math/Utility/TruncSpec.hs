module Reactor.Lib.Math.Utility.TruncSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Utility.Trunc qualified as Trunc
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Utility.Trunc (Test trunc function)" do
    describe "Trunc function" do
        it "returns 3 for trunc(3.7)" do
            let args = [Number (fromFloatDigits @Double 3.7)]
            result <- runEval (Trunc.trunc args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Trunc failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 3

        it "returns 2 for trunc(2.1)" do
            let args = [Number (fromFloatDigits @Double 2.1)]
            result <- runEval (Trunc.trunc args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Trunc failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 2

        it "returns -3 for trunc(-3.7)" do
            let args = [Number (fromFloatDigits @Double (-3.7))]
            result <- runEval (Trunc.trunc args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Trunc failed: " <> show err
                Right (res, _) -> res `shouldBe` Number (-3)

        it "returns 5 for trunc(5.0)" do
            let args = [Number 5]
            result <- runEval (Trunc.trunc args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Trunc failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 5

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Trunc.trunc args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Trunc.trunc args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Trunc.trunc args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
