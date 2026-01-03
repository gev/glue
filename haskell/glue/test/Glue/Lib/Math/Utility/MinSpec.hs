module Glue.Lib.Math.Utility.MinSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import qualified Glue.Lib.Math.Utility.Min as Min
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Utility.Min (Test min function)" do
    describe "Minimum function" do
        it "returns 2 for min(2, 5)" do
            let args = [Number 2, Number 5]
            result <- runEvalLegacy (Min.min args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Min failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 2

        it "returns 3 for min(3, 3)" do
            let args = [Number 3, Number 3]
            result <- runEvalLegacy (Min.min args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Min failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 3

        it "returns -5 for min(-5, -2)" do
            let args = [Number (-5), Number (-2)]
            result <- runEvalLegacy (Min.min args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Min failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number (-5)

        it "returns 1 for min(1, 10)" do
            let args = [Number 1, Number 10]
            result <- runEvalLegacy (Min.min args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Min failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 1

        it "fails with non-numbers (first arg)" do
            let args = [String "hello", Number 2]
            result <- runEvalLegacy (Min.min args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers (second arg)" do
            let args = [Number 2, String "hello"]
            result <- runEvalLegacy (Min.min args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (one)" do
            let args = [Number 2]
            result <- runEvalLegacy (Min.min args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments (three)" do
            let args = [Number 2, Number 3, Number 4]
            result <- runEvalLegacy (Min.min args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalLegacy (Min.min args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
