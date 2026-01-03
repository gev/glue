module Glue.Lib.Bool.WhileSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Bool.While (while_)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.While (Test while special form)" do
    describe "Loop while condition" do
        it "returns nothing when condition is false and no body" do
            let args = [Symbol "false"] -- No body, should return nothing
            result <- runEvalLegacy (while_ args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "While failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Nothing

        it "executes body and modifies environment flag" do
            -- Set up environment with flag = true
            let initialEnv = E.defineVar "flag" (Symbol "true") ((E.fromFrame lib))
            -- while flag: set flag to false
            let args = [Symbol "flag", List [Symbol "set", Symbol "flag", Symbol "false"]]
            result <- runEvalLegacy (while_ args) initialEnv
            case result of
                Left err -> expectationFailure $ "While failed: " <> show err
                Right (res, finalEnv, _) -> do
                    res `shouldBe` Nothing
                    -- Check that flag was changed to false
                    E.lookupLocal "flag" finalEnv `shouldBe` Just (Symbol "false")

        it "fails with wrong number of arguments" do
            let args = [] -- No condition
            result <- runEvalLegacy (while_ args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
