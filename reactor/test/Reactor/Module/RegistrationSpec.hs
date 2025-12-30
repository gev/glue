module Reactor.Module.RegistrationSpec where

import Reactor.Eval (Eval)
import Reactor.IR (IR (..))
import Reactor.Module (RegisteredModule (..))
import Reactor.Module.Registration (buildRegistry, registerModule, registerModules)
import Reactor.Module.Registry qualified as Registry
import Test.Hspec

spec :: Spec
spec = do
    describe "Pure module registration" $ do
        it "registers module with export collection" $ do
            let registry = Registry.emptyRegistry :: Registry.ModuleRegistry (IR Eval)
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.math"
                        , List [Symbol "export", Symbol "add", Symbol "multiply"]
                        , List [Symbol "def", Symbol "add", List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]]]
                        , List [Symbol "def", Symbol "multiply", List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "*", Symbol "a", Symbol "b"]]]
                        ]

            -- Register the module
            case registerModule registry moduleIR of
                Right newRegistry -> do
                    -- Check that module was registered
                    case Registry.lookupModule "test.math" newRegistry of
                        Just mod -> do
                            name mod `shouldBe` "test.math"
                            exports mod `shouldBe` ["add", "multiply"]
                            length (body mod) `shouldBe` 2 -- Two def forms
                        Nothing -> expectationFailure "Module not registered"
                Left err -> expectationFailure $ "Registration failed: " ++ show err

        it "handles module without exports" $ do
            let registry = Registry.emptyRegistry :: Registry.ModuleRegistry (IR Eval)
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.empty"
                        , List [Symbol "def", Symbol "x", Number 42]
                        ]

            case registerModule registry moduleIR of
                Right newRegistry -> do
                    case Registry.lookupModule "test.empty" newRegistry of
                        Just mod -> do
                            name mod `shouldBe` "test.empty"
                            exports mod `shouldBe` []
                            length (body mod) `shouldBe` 1
                        Nothing -> expectationFailure "Module not registered"
                Left err -> expectationFailure $ "Registration failed: " ++ show err

        it "builds registry from multiple modules" $ do
            let modules =
                    [ List [Symbol "module", Symbol "math.add", List [Symbol "export", Symbol "add"], List [Symbol "def", Symbol "add", Number 1]]
                    , List [Symbol "module", Symbol "math.mul", List [Symbol "export", Symbol "mul"], List [Symbol "def", Symbol "mul", Number 2]]
                    ]

            case buildRegistry modules of
                Right registry -> do
                    Registry.registrySize registry `shouldBe` 2
                    Registry.lookupModule "math.add" registry `shouldNotBe` Nothing
                    Registry.lookupModule "math.mul" registry `shouldNotBe` Nothing
                Left err -> expectationFailure $ "Registry build failed: " ++ show err

        it "rejects duplicate module names" $ do
            let registry = Registry.emptyRegistry :: Registry.ModuleRegistry (IR Eval)
            let moduleIR1 = List [Symbol "module", Symbol "test.dup", List [Symbol "export", Symbol "x"], List [Symbol "def", Symbol "x", Number 1]]
            let moduleIR2 = List [Symbol "module", Symbol "test.dup", List [Symbol "export", Symbol "y"], List [Symbol "def", Symbol "y", Number 2]]

            case registerModules registry [moduleIR1, moduleIR2] of
                Right _ -> expectationFailure "Should have rejected duplicate module"
                Left _ -> pure () -- Expected error
