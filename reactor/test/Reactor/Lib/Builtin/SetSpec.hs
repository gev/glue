module Reactor.Lib.Builtin.SetSpec (spec) where

import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib.Builtin.Set (set)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Builtin.Set (Test set special form)" do
    describe "Updating variables" do
        it "updates an existing variable" do
            let initialEnv = E.fromList [("x", Number 10)]
            let args = [Symbol "x", Number 20]
            result <- runEval (set args) initialEnv
            case result of
                Left err -> expectationFailure $ "Set failed: " <> show err
                Right (res, finalEnv, _) -> do
                    res `shouldBe` Nothing
                    E.lookupVar "x" finalEnv `shouldBe` Right (Number 20)

        it "fails to set unbound variable" do
            let initialEnv = E.emptyEnv
            let args = [Symbol "x", Number 42]
            result <- runEval (set args) initialEnv
            result `shouldSatisfy` isLeft

    describe "Setting object properties" do
        it "sets a property on an object" do
            let obj = Object (Map.fromList [("a", Number 1)])
            let initialEnv = E.fromList [("obj", obj)]
            let args = [PropAccess (Symbol "obj") "b", Number 2]
            result <- runEval (set args) initialEnv
            case result of
                Left err -> expectationFailure $ "Set failed: " <> show err
                Right (res, finalEnv, _) -> do
                    res `shouldBe` Nothing
                    case E.lookupVar "obj" finalEnv of
                        Right (Object newMap) -> do
                            Map.lookup "a" newMap `shouldBe` Just (Number 1)
                            Map.lookup "b" newMap `shouldBe` Just (Number 2)
                        _ -> expectationFailure "Object not found or not an object"

        it "fails to set property on non-object" do
            let initialEnv = E.fromList [("x", Number 10)]
            let args = [PropAccess (Symbol "x") "prop", Number 42]
            result <- runEval (set args) initialEnv
            result `shouldSatisfy` isLeft

    describe "Error cases" do
        it "fails with wrong number of arguments" do
            let initialEnv = E.emptyEnv
            let args = [Symbol "x"]
            result <- runEval (set args) initialEnv
            result `shouldSatisfy` isLeft
