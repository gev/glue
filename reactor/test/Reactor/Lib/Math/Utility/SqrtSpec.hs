module Reactor.Lib.Math.Utility.SqrtSpec (spec) where

import Data.Either (isLeft)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import qualified Reactor.Lib.Math.Utility.Sqrt as Sqrt
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Math.Utility.Sqrt (Test sqrt function)" do
    describe "Square root function" do
        it "returns 2 for sqrt(4)" do
            let args = [Number 4]
            result <- runEval (Sqrt.sqrt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sqrt failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 2

        it "returns 3 for sqrt(9)" do
            let args = [Number 9]
            result <- runEval (Sqrt.sqrt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sqrt failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 3

        it "returns 0 for sqrt(0)" do
            let args = [Number 0]
            result <- runEval (Sqrt.sqrt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Sqrt failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 0

        it "fails with negative numbers" do
            let args = [Number (-4)]
            result <- runEval (Sqrt.sqrt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEval (Sqrt.sqrt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 1, Number 2]
            result <- runEval (Sqrt.sqrt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEval (Sqrt.sqrt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
