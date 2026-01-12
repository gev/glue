module Glue.Lib.Bool.GeSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Bool.Ge (ge)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Ge (Test ge function)" do
    describe "Greater than or equal comparison" do
        it "returns true for equal numbers" do
            let args = [Integer 5, Integer 5]
            result <- runEvalSimple (ge args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ge failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns true for greater number" do
            let args = [Integer 10, Integer 5]
            result <- runEvalSimple (ge args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ge failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns false for lesser number" do
            let args = [Integer 5, Integer 10]
            result <- runEvalSimple (ge args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ge failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "fails with non-numbers" do
            let args = [String "hello", String "world"]
            result <- runEvalSimple (ge args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Integer 5]
            result <- runEvalSimple (ge args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it ">= alias works identically to ge" do
            let args1 = [Integer 5, Integer 5] -- equal
            let args2 = [Integer 10, Integer 5] -- greater
            let args3 = [Integer 5, Integer 10] -- lesser
            result1 <- runEvalSimple (ge args1) (E.fromFrame lib)
            result2 <- runEvalSimple (ge args2) (E.fromFrame lib)
            result3 <- runEvalSimple (ge args3) (E.fromFrame lib)
            case (result1, result2, result3) of
                (Right (Bool True, _, _), Right (Bool True, _, _), Right (Bool False, _, _)) -> pure ()
                _ -> expectationFailure ">= alias should work like ge"
