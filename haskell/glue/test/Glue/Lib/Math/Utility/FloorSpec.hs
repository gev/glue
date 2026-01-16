module Glue.Lib.Math.Utility.FloorSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Utility.Floor qualified as Floor
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Utility.Floor (Test floor function)" do
    describe "Floor function" do
        it "returns 3 for floor(3.7)" do
            let args = [Float 3.7]
            result <- runEvalSimple (Floor.floor args) []
            case result of
                Left err -> expectationFailure $ "Floor failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 3

        it "returns 2 for floor(2.1)" do
            let args = [Float 2.1]
            result <- runEvalSimple (Floor.floor args) []
            case result of
                Left err -> expectationFailure $ "Floor failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 2

        it "returns -4 for floor(-3.1)" do
            let args = [Float (-3.1)]
            result <- runEvalSimple (Floor.floor args) []
            case result of
                Left err -> expectationFailure $ "Floor failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer (-4)

        it "returns 5 for floor(5.0)" do
            let args = [Integer 5]
            result <- runEvalSimple (Floor.floor args) []
            case result of
                Left err -> expectationFailure $ "Floor failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 5

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (Floor.floor args) []
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (Floor.floor args) []
            result `shouldSatisfy` isLeft

        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (Floor.floor args) []
            result `shouldSatisfy` isLeft
