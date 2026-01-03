module Glue.Lib.Bool.UntilSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib (lib)
import Glue.Lib.Bool.Until (until_)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Bool.Until (Test until special form)" do
    describe "Loop until condition" do
        it "returns nothing when condition is true and no body" do
            let args = [Symbol "true"] -- No body, should return nothing
            result <- runEvalLegacy (until_ args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Until failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Nothing

        it "executes body and modifies environment flag" do
            -- Set up environment with flag = false
            let initialEnv = E.defineVar "flag" (Symbol "false") ((E.fromFrame lib))
            -- until flag: set flag to true
            let args = [Symbol "flag", List [Symbol "set", Symbol "flag", Symbol "true"]]
            result <- runEvalLegacy (until_ args) initialEnv
            case result of
                Left err -> expectationFailure $ "Until failed: " <> show err
                Right (res, finalEnv, _) -> do
                    res `shouldBe` Nothing
                    -- Check that flag was changed to true
                    E.lookupLocal "flag" finalEnv `shouldBe` Just (Symbol "true")

        it "fails with wrong number of arguments" do
            let args = [] -- No condition
            result <- runEvalLegacy (until_ args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
