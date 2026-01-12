module Glue.Module.RegistrationSpec where

import Glue.IR (IR (..))
import Glue.Module (RegisteredModule (..))
import Glue.Module.Registration (buildRegistry, parseModule, registerModule, registerModules)
import Glue.Module.Registry (emptyRegistry, lookupModule, registrySize)
import Test.Hspec
import Prelude hiding (mod)

spec :: Spec
spec = do
    describe "Pure module registration" $ do
        it "registers module with export collection" $ do
            let registry = emptyRegistry
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.math"
                        , List [Symbol "export", Symbol "add", Symbol "multiply"]
                        , List [Symbol "def", Symbol "add", List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]]]
                        , List [Symbol "def", Symbol "multiply", List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "*", Symbol "a", Symbol "b"]]]
                        ]

            -- Register the module
            case registerModule registry =<< parseModule moduleIR of
                Right newRegistry -> do
                    -- Check that module was registered
                    case lookupModule "test.math" newRegistry of
                        Just mod -> do
                            name mod `shouldBe` "test.math"
                            exports mod `shouldBe` ["add", "multiply"]
                            length (body mod) `shouldBe` 2 -- Two def forms
                        Nothing -> expectationFailure "Module not registered"
                Left err -> expectationFailure $ "Registration failed: " ++ show err

        it "handles module without exports" $ do
            let registry = emptyRegistry
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.empty"
                        , List [Symbol "def", Symbol "x", Integer 42]
                        ]

            case registerModule registry =<< parseModule moduleIR of
                Right newRegistry -> do
                    case lookupModule "test.empty" newRegistry of
                        Just mod -> do
                            name mod `shouldBe` "test.empty"
                            exports mod `shouldBe` []
                            length (body mod) `shouldBe` 1
                        Nothing -> expectationFailure "Module not registered"
                Left err -> expectationFailure $ "Registration failed: " ++ show err

        it "builds registry from multiple modules" $ do
            let modules =
                    [ List [Symbol "module", Symbol "math.add", List [Symbol "export", Symbol "add"], List [Symbol "def", Symbol "add", Integer 1]]
                    , List [Symbol "module", Symbol "math.mul", List [Symbol "export", Symbol "mul"], List [Symbol "def", Symbol "mul", Integer 2]]
                    ]

            case buildRegistry modules of
                Right registry -> do
                    registrySize registry `shouldBe` 2
                    lookupModule "math.add" registry `shouldNotBe` Nothing
                    lookupModule "math.mul" registry `shouldNotBe` Nothing
                Left err -> expectationFailure $ "Registry build failed: " ++ show err

        it "rejects duplicate module names" $ do
            let registry = emptyRegistry
            let moduleIR1 = List [Symbol "module", Symbol "test.dup", List [Symbol "export", Symbol "x"], List [Symbol "def", Symbol "x", Integer 1]]
            let moduleIR2 = List [Symbol "module", Symbol "test.dup", List [Symbol "export", Symbol "y"], List [Symbol "def", Symbol "y", Integer 2]]

            case registerModules registry =<< mapM parseModule [moduleIR1, moduleIR2] of
                Right _ -> expectationFailure "Should have rejected duplicate module"
                Left _ -> pure () -- Expected error
