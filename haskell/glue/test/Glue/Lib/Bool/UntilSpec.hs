module Glue.Lib.Bool.UntilSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (Runtime (..), apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Until (until_)
import Glue.Lib.Builtin (builtin)
import Glue.Module (envFromModule)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.Until (Test until special form)" do
    describe "Loop until condition" do
        it "returns nothing when condition is true and no body" do
            let args = [Bool True] -- No body, should return nothing
            result <- runEvalSimple (apply until_ args) []
            case result of
                Left err -> expectationFailure $ "Until failed: " <> show err
                Right (res, _) -> res `shouldBe` Void

        it "executes body and modifies environment flag" do
            -- Set up environment with flag = false
            let initialEnv = E.defineVar "flag" (Bool False) (envFromModule builtin)
            -- until flag: set flag to true
            let args = [Symbol "flag", List [Symbol "set", Symbol "flag", Bool True]]
            result <- runEvalSimple (apply until_ args) initialEnv
            case result of
                Left err -> expectationFailure $ "Until failed: " <> show err
                Right (res, runtime) -> do
                    res `shouldBe` Void
                    -- Check that flag was changed to true
                    E.lookupLocal "flag" runtime.env `shouldBe` Just (Bool True)
