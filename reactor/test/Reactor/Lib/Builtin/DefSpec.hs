module Reactor.Lib.Builtin.DefSpec (spec) where

import Data.Either (isLeft)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib.Builtin.Def (def)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Builtin.Def (Test def special form)" do
    describe "Defining variables" do
        it "defines a variable in the environment" do
            let initialEnv = E.emptyEnv
            let args = [Symbol "x", Number 42]
            result <- runEval (def args) initialEnv
            case result of
                Left err -> expectationFailure $ "Def failed: " <> show err
                Right (res, finalEnv, _) -> do
                    res `shouldBe` Nothing
                    E.lookupLocal "x" finalEnv `shouldBe` Just (Number 42)

        it "fails with wrong number of arguments" do
            let initialEnv = E.emptyEnv
            let args = [Symbol "x"]
            result <- runEval (def args) initialEnv
            result `shouldSatisfy` isLeft

        it "fails with non-symbol as name" do
            let initialEnv = E.emptyEnv
            let args = [Number 1, Number 42]
            result <- runEval (def args) initialEnv
            result `shouldSatisfy` isLeft
