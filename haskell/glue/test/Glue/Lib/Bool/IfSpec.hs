module Glue.Lib.Bool.IfSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Bool.If (if_)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.If (Test if special form)" do
    describe "Conditional execution" do
        it "executes then branch when condition is true" do
            let args = [Bool True, Integer 42, Integer 0]
            result <- runEvalLegacy (if_ args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "If failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Just (Integer 42)

        it "executes else branch when condition is false" do
            let args = [Bool False, Integer 42, Integer 0]
            result <- runEvalLegacy (if_ args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "If failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Just (Integer 0)

        it "fails with wrong number of arguments" do
            let args = [Bool True, Integer 42]
            result <- runEvalLegacy (if_ args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
