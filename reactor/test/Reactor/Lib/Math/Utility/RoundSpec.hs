module Reactor.Lib.Math.Utility.RoundSpec (spec) where

import Data.Either (isLeft)
import Data.Scientific (fromFloatDigits)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Math.Utility.Round qualified as Round
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Utility.Round (Test round function)" do
    describe "Round function" do
        it "returns 4 for round(3.5)" do
            let args = [Number (fromFloatDigits @Double 3.5)]
            result <- runEval (Round.round args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Round failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 4

        it "returns 2 for round(2.4)" do
            let args = [Number (fromFloatDigits @Double 2.4)]
            result <- runEval (Round.round args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Round failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 2

        it "returns -3 for round(-3.5)" do
            let args = [Number (fromFloatDigits @Double (-3.5))]
            result <- runEval (Round.round args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Round failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number (-4)

        it "returns 5 for round(5.0)" do
            let args = [Number 5]
            result <- runEval (Round.round args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Round failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 5

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Round.round args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Round.round args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Round.round args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
