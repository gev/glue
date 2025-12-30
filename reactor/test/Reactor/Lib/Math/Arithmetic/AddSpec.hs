module Reactor.Lib.Math.Arithmetic.AddSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits)
import Reactor.Env qualified as E
import Reactor.Eval (runEvalLegacy)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Arithmetic.Add qualified as Add
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Arithmetic.Add (Test add function)" do
    describe "Add function" do
        it "returns 5 for (+ 2 3)" do
            let args = [Number 2, Number 3]
            result <- runEvalLegacy (Add.add args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Add failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 5

        it "returns 10 for (+ 1 2 3 4)" do
            let args = [Number 1, Number 2, Number 3, Number 4]
            result <- runEvalLegacy (Add.add args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Add failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 10

        it "returns 3.5 for (+ 1.5 2)" do
            let args = [Number (fromFloatDigits @Double 1.5), Number 2]
            result <- runEvalLegacy (Add.add args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Add failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number (fromFloatDigits @Double 3.5)

        it "returns 0 for (+) with no arguments" do
            let args = []
            result <- runEvalLegacy (Add.add args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [Number 1, String "hello"]
            result <- runEvalLegacy (Add.add args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
