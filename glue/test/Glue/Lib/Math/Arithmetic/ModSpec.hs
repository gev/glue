module Glue.Lib.Math.Arithmetic.ModSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Math.Arithmetic.Mod qualified as Mod
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Arithmetic.Mod (Test mod function)" do
    describe "Mod function" do
        it "returns 1 for (% 7 3)" do
            let args = [Number 7, Number 3]
            result <- runEvalLegacy (Mod.mod args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mod failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 1

        it "returns 0 for (% 6 3)" do
            let args = [Number 6, Number 3]
            result <- runEvalLegacy (Mod.mod args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mod failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 0

        it "returns 2 for (% 17 5)" do
            let args = [Number 17, Number 5]
            result <- runEvalLegacy (Mod.mod args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mod failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Number 2

        it "fails with wrong number of arguments" do
            let args = [Number 7]
            result <- runEvalLegacy (Mod.mod args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with division by zero" do
            let args = [Number 7, Number 0]
            result <- runEvalLegacy (Mod.mod args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [Number 7, String "hello"]
            result <- runEvalLegacy (Mod.mod args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
