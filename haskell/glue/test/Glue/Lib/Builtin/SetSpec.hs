module Glue.Lib.Builtin.SetSpec (spec) where

import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Glue.Env qualified as E
import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Builtin.Set (set)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Builtin.Set (Test set special form)" do
    describe "Updating variables" do
        it "updates an existing variable" do
            let initialEnv = E.fromList [("x", Integer 10)]
            let args = [Symbol "x", Integer 20]
            result <- runEvalSimple (set args) initialEnv
            case result of
                Left err -> expectationFailure $ "Set failed: " <> show err
                Right (res, finalEnv, _) -> do
                    res `shouldBe` Void
                    E.lookupVar "x" finalEnv `shouldBe` Right (Integer 20)

        it "fails to set unbound variable" do
            let args = [Symbol "x", Integer 42]
            result <- runEvalSimple (set args) []
            result `shouldSatisfy` isLeft

    describe "Setting object properties" do
        it "sets a property on an object" do
            let obj = Object (Map.fromList [("a", Integer 1)])
            let initialEnv = E.fromList [("obj", obj)]
            let args = [Symbol "obj.b", Integer 2]
            result <- runEvalSimple (set args) initialEnv
            case result of
                Left err -> expectationFailure $ "Set failed: " <> show err
                Right (res, finalEnv, _) -> do
                    res `shouldBe` Void
                    case E.lookupVar "obj" finalEnv of
                        Right (Object newMap) -> do
                            Map.lookup "a" newMap `shouldBe` Just (Integer 1)
                            Map.lookup "b" newMap `shouldBe` Just (Integer 2)
                        _ -> expectationFailure "Object not found or not an object"

        it "fails to set property on non-object" do
            let initialEnv = E.fromList [("x", Integer 10)]
            let args = [Symbol "x.prop", Integer 42]
            result <- runEvalSimple (set args) initialEnv
            result `shouldSatisfy` isLeft

    describe "Error cases" do
        it "fails with wrong number of arguments" do
            let args = [Symbol "x"]
            result <- runEvalSimple (set args) []
            result `shouldSatisfy` isLeft
