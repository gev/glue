module Glue.Lib.Builtin.DefSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib.Builtin.Def (def)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Builtin.Def (Test def special form)" do
    describe "Defining variables" do
        it "defines a variable in the environment" do
            let initialEnv = E.emptyEnv
            let args = [Symbol "x", Integer 42]
            result <- runEvalLegacy (def args) initialEnv
            case result of
                Left err -> expectationFailure $ "Def failed: " <> show err
                Right (res, finalEnv, _) -> do
                    res `shouldBe` Nothing
                    E.lookupLocal "x" finalEnv `shouldBe` Just (Integer 42)

        it "fails with wrong number of arguments" do
            let initialEnv = E.emptyEnv
            let args = [Symbol "x"]
            result <- runEvalLegacy (def args) initialEnv
            result `shouldSatisfy` isLeft

        it "fails with non-symbol as name" do
            let initialEnv = E.emptyEnv
            let args = [Integer 1, Integer 42]
            result <- runEvalLegacy (def args) initialEnv
            result `shouldSatisfy` isLeft
