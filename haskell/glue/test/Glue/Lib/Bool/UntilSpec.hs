module Glue.Lib.Bool.UntilSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Bool.Until (until_)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Until (Test until special form)" do
    describe "Loop until condition" do
        it "returns nothing when condition is true and no body" do
            let args = [Bool True] -- No body, should return nothing
            result <- runEvalLegacy (until_ args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Until failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Void

        it "executes body and modifies environment flag" do
            -- Set up environment with flag = false
            let initialEnv = E.defineVar "flag" (Bool False) (E.fromFrame lib)
            -- until flag: set flag to true
            let args = [Symbol "flag", List [Symbol "set", Symbol "flag", Bool True]]
            result <- runEvalLegacy (until_ args) initialEnv
            case result of
                Left err -> expectationFailure $ "Until failed: " <> show err
                Right (res, finalEnv, _) -> do
                    res `shouldBe` Void
                    -- Check that flag was changed to true
                    E.lookupLocal "flag" finalEnv `shouldBe` Just (Bool True)

        it "fails with wrong number of arguments" do
            let args = [] -- No condition
            result <- runEvalLegacy (until_ args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
