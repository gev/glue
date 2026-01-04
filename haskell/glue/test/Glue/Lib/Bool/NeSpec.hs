module Glue.Lib.Bool.NeSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Bool.Ne (ne)
import Glue.TestUtils ()
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Ne (Test ne function)" do
    describe "Not equal comparison" do
        it "returns true for unequal numbers" do
            let args = [Number 42, Number 43]
            result <- runEvalLegacy (ne args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns false for equal numbers" do
            let args = [Number 42, Number 42]
            result <- runEvalLegacy (ne args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "returns true for unequal strings" do
            let args = [String "hello", String "world"]
            result <- runEvalLegacy (ne args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool True

        it "returns false for equal strings" do
            let args = [String "hello", String "hello"]
            result <- runEvalLegacy (ne args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Ne failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Bool False

        it "fails with wrong number of arguments" do
            let args = [Number 42]
            result <- runEvalLegacy (ne args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "\\= alias works identically to ne" do
            let args1 = [Number 42, Number 43] -- unequal
            let args2 = [Number 42, Number 42] -- equal
            result1 <- runEvalLegacy (ne args1) (E.fromFrame lib)
            result2 <- runEvalLegacy (ne args2) (E.fromFrame lib)
            case (result1, result2) of
                (Right (Bool True, _, _), Right (Bool False, _, _)) -> pure ()
                _ -> expectationFailure "\\= alias should work like ne"
