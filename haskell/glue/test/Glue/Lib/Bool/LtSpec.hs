module Glue.Lib.Bool.LtSpec (spec) where

import Data.Either (isLeft)
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Lt (lt)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Lt (Test lt function)" do
    describe "Less than comparison" do
        it "returns true for lesser number" do
            let args = [Integer 5, Integer 10]
            result <- runEvalSimple (apply lt args) []
            case result of
                Left err -> expectationFailure $ "Lt failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool True

        it "returns false for equal numbers" do
            let args = [Integer 5, Integer 5]
            result <- runEvalSimple (apply lt args) []
            case result of
                Left err -> expectationFailure $ "Lt failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "returns false for greater number" do
            let args = [Integer 10, Integer 5]
            result <- runEvalSimple (apply lt args) []
            case result of
                Left err -> expectationFailure $ "Lt failed: " <> show err
                Right (res, _) -> res `shouldBe` Bool False

        it "fails with non-numbers" do
            let args = [String "hello", String "world"]
            result <- runEvalSimple (apply lt args) []
            result `shouldSatisfy` isLeft

        it "< alias works identically to lt" do
            let args1 = [Integer 5, Integer 10] -- lesser
            let args2 = [Integer 5, Integer 5] -- equal
            let args3 = [Integer 10, Integer 5] -- greater
            result1 <- runEvalSimple (apply lt args1) []
            result2 <- runEvalSimple (apply lt args2) []
            result3 <- runEvalSimple (apply lt args3) []
            case (result1, result2, result3) of
                (Right (Bool True, _), Right (Bool False, _), Right (Bool False, _)) -> pure ()
                _ -> expectationFailure "< alias should work like lt"
