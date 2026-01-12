module Glue.Lib.Bool.LtSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Bool.Lt (lt)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Lt (Test lt function)" do
    describe "Less than comparison" do
        it "returns true for lesser number" do
            let args = [Integer 5, Integer 10]
            result <- runEvalSimple (lt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns false for equal numbers" do
            let args = [Integer 5, Integer 5]
            result <- runEvalSimple (lt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "returns false for greater number" do
            let args = [Integer 10, Integer 5]
            result <- runEvalSimple (lt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Lt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "fails with non-numbers" do
            let args = [String "hello", String "world"]
            result <- runEvalSimple (lt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 5]
            result <- runEvalSimple (lt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "< alias works identically to lt" do
            let args1 = [Integer 5, Integer 10] -- lesser
            let args2 = [Integer 5, Integer 5] -- equal
            let args3 = [Integer 10, Integer 5] -- greater
            result1 <- runEvalSimple (lt args1) (E.fromFrame lib)
            result2 <- runEvalSimple (lt args2) (E.fromFrame lib)
            result3 <- runEvalSimple (lt args3) (E.fromFrame lib)
            case (result1, result2, result3) of
                (Right (Bool True, _, _), Right (Bool False, _, _), Right (Bool False, _, _)) -> pure ()
                _ -> expectationFailure "< alias should work like lt"
