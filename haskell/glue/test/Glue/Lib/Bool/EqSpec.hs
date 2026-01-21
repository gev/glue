module Glue.Lib.Bool.EqSpec (spec) where

import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Eq (eq)
import Glue.TestUtils ()
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Eq (Test eq function)" do
    describe "Equality comparison" do
        it "returns true for equal numbers" do
            result <- runEvalSimple (apply eq [Integer 42, Integer 42]) []
            case result of
                Left err -> expectationFailure $ "Eq failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool True

        it "returns false for unequal numbers" do
            result <- runEvalSimple (apply eq [Integer 42, Integer 43]) []
            case result of
                Left err -> expectationFailure $ "Eq failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "returns true for equal strings" do
            result <- runEvalSimple (apply eq [String "hello", String "hello"]) []
            case result of
                Left err -> expectationFailure $ "Eq failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool True

        it "returns false for unequal strings" do
            result <- runEvalSimple (apply eq [String "hello", String "world"]) []
            case result of
                Left err -> expectationFailure $ "Eq failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False
