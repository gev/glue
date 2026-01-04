module Glue.Lib.Bool.GtSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Bool.Gt (gt)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Gt (Test gt function)" do
    describe "Greater than comparison" do
        it "returns true for greater number" do
            let args = [Number 10, Number 5]
            result <- runEvalLegacy (gt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Gt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns false for equal numbers" do
            let args = [Number 5, Number 5]
            result <- runEvalLegacy (gt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Gt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "returns false for lesser number" do
            let args = [Number 5, Number 10]
            result <- runEvalLegacy (gt args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Gt failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "fails with non-numbers" do
            let args = [String "hello", String "world"]
            result <- runEvalLegacy (gt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with wrong number of arguments" do
            let args = [Number 5]
            result <- runEvalLegacy (gt args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "> alias works identically to gt" do
            let args1 = [Number 10, Number 5] -- greater
            let args2 = [Number 5, Number 5] -- equal
            let args3 = [Number 5, Number 10] -- lesser
            result1 <- runEvalLegacy (gt args1) (E.fromFrame lib)
            result2 <- runEvalLegacy (gt args2) (E.fromFrame lib)
            result3 <- runEvalLegacy (gt args3) (E.fromFrame lib)
            case (result1, result2, result3) of
                (Right (Bool True, _, _), Right (Bool False, _, _), Right (Bool False, _, _)) -> pure ()
                _ -> expectationFailure "> alias should work like gt"
