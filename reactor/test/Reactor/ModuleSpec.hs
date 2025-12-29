module Reactor.ModuleSpec where

import Control.Monad (void)
import Data.IORef (readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Reactor.Eval (Eval, runEval)
import Reactor.Env qualified as E
import Reactor.IR (IR (..))
import Reactor.Module (Module (..), ModuleRegistry)
import Reactor.Module.Registration (RegistryRef, newRegistry, registerModuleFromIR)
import Reactor.Module.System (libWithModules)
import Test.Hspec

spec :: Spec
spec = do
    describe "Module data structure" $ do
        it "creates module correctly" $ do
            let mod = Module
                    { name = "test.module"
                    , exports = ["func1", "func2"]
                    , body = [Symbol "def", Symbol "func1", Number 42]
                    }
            name mod `shouldBe` "test.module"
            exports mod `shouldBe` ["func1", "func2"]
            length (body mod) `shouldBe` 3

    describe "ModuleRegistry operations" $ do
        it "empty registry" $ do
            let registry = Map.empty :: ModuleRegistry Eval
            Map.size registry `shouldBe` 0

        it "adds and retrieves modules" $ do
            let mod = Module "test.mod" ["x"] [Number 1]
                registry = Map.singleton "test.mod" mod
            case Map.lookup "test.mod" registry of
                Just m -> name m `shouldBe` "test.mod"
                Nothing -> expectationFailure "Module not found"

        it "handles missing modules" $ do
            let registry = Map.empty :: ModuleRegistry Eval
            Map.lookup "nonexistent" registry `shouldBe` Nothing

    describe "Evaluation-based module registration" $ do
        it "registers module with export collection" $ do
            registry <- newRegistry
            let moduleIR = List
                    [ Symbol "module"
                    , Symbol "test.math"
                    , List [Symbol "export", Symbol "add", Symbol "multiply"]
                    , List [Symbol "def", Symbol "add", List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]]]
                    , List [Symbol "def", Symbol "multiply", List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "*", Symbol "a", Symbol "b"]]]
                    ]

            -- Register the module
            let env = E.fromFrame (libWithModules registry)
            result <- runEval (registerModuleFromIR registry moduleIR) env

            case result of
                Right (_, _, _) -> do
                    -- Check that module was registered
                    regMap <- readIORef registry
                    case Map.lookup "test.math" regMap of
                        Just mod -> do
                            name mod `shouldBe` "test.math"
                            exports mod `shouldBe` ["add", "multiply"]
                            length (body mod) `shouldBe` 2  -- Two def forms
                        Nothing -> expectationFailure "Module not registered"
                Left err -> expectationFailure $ "Registration failed: " ++ show err

        it "handles module without exports" $ do
            registry <- newRegistry
            let moduleIR = List
                    [ Symbol "module"
                    , Symbol "test.empty"
                    , List [Symbol "def", Symbol "x", Number 42]
                    ]

            let env = E.fromFrame (libWithModules registry)
            result <- runEval (registerModuleFromIR registry moduleIR) env

            case result of
                Right (_, _, _) -> do
                    regMap <- readIORef registry
                    case Map.lookup "test.empty" regMap of
                        Just mod -> do
                            name mod `shouldBe` "test.empty"
                            exports mod `shouldBe` []
                            length (body mod) `shouldBe` 1
                        Nothing -> expectationFailure "Module not registered"
                Left err -> expectationFailure $ "Registration failed: " ++ show err
