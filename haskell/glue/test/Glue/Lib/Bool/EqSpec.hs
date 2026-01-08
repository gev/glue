module Glue.Lib.Bool.EqSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Eq (eq)
import Glue.TestUtils ()
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Eq (Test eq function)" do
    describe "Equality comparison" do
        it "returns true for equal numbers" do
            let args = [Integer 42, Integer 42]
            result <- runEvalLegacy (eq args) []
            case result of
                Left err -> expectationFailure $ "Eq failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns false for unequal numbers" do
            let args = [Integer 42, Integer 43]
            result <- runEvalLegacy (eq args) []
            case result of
                Left err -> expectationFailure $ "Eq failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "returns true for equal strings" do
            let args = [String "hello", String "hello"]
            result <- runEvalLegacy (eq args) []
            case result of
                Left err -> expectationFailure $ "Eq failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns false for unequal strings" do
            let args = [String "hello", String "world"]
            result <- runEvalLegacy (eq args) []
            case result of
                Left err -> expectationFailure $ "Eq failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "fails with wrong number of arguments" do
            let args = [Integer 42]
            result <- runEvalLegacy (eq args) []
            result `shouldSatisfy` isLeft
