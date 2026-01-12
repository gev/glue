module Glue.Lib.Bool.WhileSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.While (while_)
import Glue.Lib.Builtin (builtin)
import Glue.Module (envFromModule)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.While (Test while special form)" do
    describe "Loop while condition" do
        it "returns nothing when condition is false and no body" do
            let args = [Bool False] -- No body, should return nothing
            result <- runEvalSimple (while_ args) []
            case result of
                Left err -> expectationFailure $ "While failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Void

        it "executes body and modifies environment flag" do
            -- Set up environment with flag = true
            let initialEnv = E.defineVar "flag" (Bool True) (envFromModule builtin)
            -- while flag: set flag to false
            let args = [Symbol "flag", List [Symbol "set", Symbol "flag", Bool False]]
            result <- runEvalSimple (while_ args) initialEnv
            case result of
                Left err -> expectationFailure $ "While failed: " <> show err
                Right (res, finalEnv, _) -> do
                    res `shouldBe` Void
                    -- Check that flag was changed to false
                    E.lookupLocal "flag" finalEnv `shouldBe` Just (Bool False)

        it "fails with wrong number of arguments" do
            let args = [] -- No condition
            result <- runEvalSimple (while_ args) []
            result `shouldSatisfy` isLeft
