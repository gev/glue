module Glue.Lib.Math.Utility.CeilSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Math.Utility.Ceil (ceil)
import Glue.TestUtils ()
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Math.Utility.Ceil (Test ceil function)" do
    describe "Ceil function" do
        it "returns 4 for ceil(3.1)" do
            let args = [Float 3.1]
            result <- runEvalSimple (apply ceil args) []
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 4

        it "returns 3 for ceil(2.9)" do
            let args = [Float 2.9]
            result <- runEvalSimple (apply ceil args) []
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 3

        it "returns -3 for ceil(-3.1)" do
            let args = [Float (-3.1)]
            result <- runEvalSimple (apply ceil args) []
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer (-3)

        it "returns 5 for ceil(5.0)" do
            let args = [Integer 5]
            result <- runEvalSimple (apply ceil args) []
            case result of
                Left err -> expectationFailure $ "Ceil failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 5

        it "fails with non-numbers" do
            let args = [String "hello"]
            result <- runEvalSimple (apply ceil args) []
            result `shouldSatisfy` isLeft
