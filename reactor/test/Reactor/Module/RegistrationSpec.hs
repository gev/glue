module Reactor.Module.RegistrationSpec where

import Data.IORef (readIORef)
import Data.Map.Strict qualified as Map
import Reactor.Eval (Eval)
import Reactor.Module (Module (..), ModuleRegistry, lookupModule)
import Reactor.Module.Registration (newRegistry, registerModuleFromIR)
import Reactor.IR (IR (..))
import Test.Hspec
import Prelude hiding (mod)

spec :: Spec
spec = do
    describe "Module data structure" $ do
        it "creates module correctly" $ do
            let mod =
                    Module
                        { name = "test.module"
                        , exports = ["func1", "func2"]
                        , body = [Symbol "def", Symbol "func1", Number 42]
                        }
            name mod `shouldBe` "test.module"
            exports mod `shouldBe` ["func1", "func2"]
            length (body mod) `shouldBe` 3

    describe "ModuleRegistry operations" $ do
        it "handles missing modules" $ do
            let registry = Map.empty :: ModuleRegistry Eval
            lookupModule "nonexistent" registry `shouldBe` Nothing

    describe "Evaluation-based module registration" $ do
        it "registers module with export collection" $ do
            registry <- newRegistry
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.math"
                        , List [Symbol "export", Symbol "add", Symbol "multiply"]
                        , List [Symbol "def", Symbol "add", List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]]]
                        , List [Symbol "def", Symbol "multiply", List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "*", Symbol "a", Symbol "b"]]]
                        ]

            -- Register the module
            result <- registerModuleFromIR registry moduleIR

            case result of
                Right () -> do
                    -- Check that module was registered
                    regMap <- readIORef registry
                    case Map.lookup "test.math" regMap of
                        Just mod -> do
                            name mod `shouldBe` "test.math"
                            exports mod `shouldBe` ["add", "multiply"]
                            length (body mod) `shouldBe` 2 -- Two def forms
                        Nothing -> expectationFailure "Module not registered"
                Left err -> expectationFailure $ "Registration failed: " ++ show err

        it "handles module without exports" $ do
            registry <- newRegistry
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.empty"
                        , List [Symbol "def", Symbol "x", Number 42]
                        ]

            result <- registerModuleFromIR registry moduleIR

            case result of
                Right () -> do
                    regMap <- readIORef registry
                    case Map.lookup "test.empty" regMap of
                        Just mod -> do
                            name mod `shouldBe` "test.empty"
                            exports mod `shouldBe` []
                            length (body mod) `shouldBe` 1
                        Nothing -> expectationFailure "Module not registered"
                Left err -> expectationFailure $ "Registration failed: " ++ show err
