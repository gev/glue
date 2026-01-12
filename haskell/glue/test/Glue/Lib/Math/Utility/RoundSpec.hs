module Glue.Lib.Math.Utility.RoundSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Utility.Round qualified as Round
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Utility.Round (Test round function)" do
    describe "Round function" do
        it "returns 4 for round(3.5)" do
            let args = [Float 3.5]
            result <- runEvalSimple (Round.round args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Round failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 4

        it "returns 2 for round(2.4)" do
            let args = [Float 2.4]
            result <- runEvalSimple (Round.round args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Round failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 2

        it "returns -3 for round(-3.5)" do
            let args = [Float (-3.5)]
            result <- runEvalSimple (Round.round args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Round failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer (-4)

        it "returns 5 for round(5.0)" do
            let args = [Integer 5]
            result <- runEvalSimple (Round.round args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Round failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Integer 5

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (Round.round args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (Round.round args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Round.round args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
