module Reactor.Lib.Builtin.ObjectSpec (spec) where

import Data.Map.Strict qualified as Map
import Reactor.Env qualified as E
import Reactor.Eval (runEvalLegacy)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Builtin.Object (object)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Builtin.Object (Test object special form)" do
    describe "Creating objects" do
        it "creates an empty object" do
            let initialEnv = E.emptyEnv
            let args = []
            result <- runEvalLegacy (object args) initialEnv
            case result of
                Left err -> expectationFailure $ "Object failed: " <> show err
                Right (Just res, _, _) -> res `shouldBe` Object Map.empty
                Right (Nothing, _, _) -> expectationFailure "Object returned Nothing"

        it "handles existing object (backward compatibility)" do
            let initialEnv = E.emptyEnv
            let existingObj = Object (Map.fromList [("a", Number 1)])
            let args = [existingObj]
            result <- runEvalLegacy (object args) initialEnv
            case result of
                Left err -> expectationFailure $ "Object failed: " <> show err
                Right (Just res, _, _) -> res `shouldBe` existingObj
                Right (Nothing, _, _) -> expectationFailure "Object returned Nothing"

        it "handles nested objects with unevaluated expressions" do
            let initialEnv = E.fromFrame lib
            let nestedObj = Object (Map.fromList [("x", List [Symbol "object", Object (Map.fromList [("y", List [Symbol "object", Object (Map.fromList [("z", Number 1)])])])])])
            let args = [nestedObj]
            result <- runEvalLegacy (object args) initialEnv
            case result of
                Left err -> expectationFailure $ "Object failed: " <> show err
                Right (Just res, _, _) -> res `shouldBe` Object (Map.fromList [("x", Object (Map.fromList [("y", Object (Map.fromList [("z", Number 1)]))]))])
                Right (Nothing, _, _) -> expectationFailure "Object returned Nothing"
