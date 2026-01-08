module Glue.Lib.List.LengthSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib.List.Length qualified as Length
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Length (Test length function)" do
    it "returns 0 for empty list" do
        let initialEnv = E.emptyEnv
        let args = [List []]
        result <- runEvalLegacy (Length.length args) initialEnv
        case result of
            Left err -> expectationFailure $ "Length failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Integer 0

    it "returns correct length for non-empty list" do
        let initialEnv = E.emptyEnv
        let args = [List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalLegacy (Length.length args) initialEnv
        case result of
            Left err -> expectationFailure $ "Length failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Integer 3

    it "returns length for list with mixed types" do
        let initialEnv = E.emptyEnv
        let args = [List [Integer 42, String "hello", Float 3.14]]
        result <- runEvalLegacy (Length.length args) initialEnv
        case result of
            Left err -> expectationFailure $ "Length failed: " <> show err
            Right (res, _, _) -> res `shouldBe` Integer 3

    it "fails on non-list" do
        let initialEnv = E.emptyEnv
        let args = [Integer 42]
        result <- runEvalLegacy (Length.length args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Length should fail on non-list"
