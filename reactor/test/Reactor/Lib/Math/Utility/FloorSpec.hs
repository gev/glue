module Reactor.Lib.Math.Utility.FloorSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Utility.Floor qualified as Floor
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Utility.Floor (Test floor function)" do
    describe "Floor function" do
        it "returns 3 for floor(3.7)" do
            let args = [Number (fromFloatDigits @Double 3.7)]
            result <- runEval (Floor.floor args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Floor failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 3

        it "returns 2 for floor(2.1)" do
            let args = [Number (fromFloatDigits @Double 2.1)]
            result <- runEval (Floor.floor args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Floor failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 2

        it "returns -4 for floor(-3.1)" do
            let args = [Number (fromFloatDigits @Double (-3.1))]
            result <- runEval (Floor.floor args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Floor failed: " <> show err
                Right (res, _) -> res `shouldBe` Number (-4)

        it "returns 5 for floor(5.0)" do
            let args = [Number 5]
            result <- runEval (Floor.floor args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Floor failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 5

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Floor.floor args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Floor.floor args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Floor.floor args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
