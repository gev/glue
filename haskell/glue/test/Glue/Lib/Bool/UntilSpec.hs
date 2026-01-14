module Glue.Lib.Bool.UntilSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (Runtime (..), runEvalSimple)
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
            result <- runEvalSimple (until_ args) []
            case result of
                Left err -> expectationFailure $ "Until failed: " <> show err
                Right (res, _) -> res `shouldBe` Void

        it "executes body and modifies environment flag" do
            -- Set up environment with flag = false
            let initialEnv = E.defineVar "flag" (Bool False) (envFromModule builtin)
            -- until flag: set flag to true
            let args = [Symbol "flag", List [Symbol "set", Symbol "flag", Bool True]]
            result <- runEvalSimple (until_ args) initialEnv
            case result of
                Left err -> expectationFailure $ "Until failed: " <> show err
                Right (res, runtime) -> do
                    res `shouldBe` Void
                    -- Check that flag was changed to true
                    E.lookupLocal "flag" runtime.env `shouldBe` Just (Bool True)

        it "fails with wrong number of arguments" do
            let args = [] -- No condition
            result <- runEvalSimple (until_ args) []
            result `shouldSatisfy` isLeft
