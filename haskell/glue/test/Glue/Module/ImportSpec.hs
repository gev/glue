module Glue.Module.ImportSpec where

import Data.Map.Strict qualified as Map
import Glue.Env qualified as E
import Glue.Eval (Runtime (..), eval, runEval)
import Glue.IR (IR (..))
import Glue.Lib.Builtin (builtin)
import Glue.Lib.Math.Arithmetic (arithmetic)
import Glue.Module (envFromModule, envFromModules)
import Glue.Module.Cache qualified as Cache
import Glue.Module.Registration (buildRegistry)
import Test.Hspec

spec :: Spec
spec = do
    describe "Module import functionality" $ do
        it "imports module and makes exported symbols available" $ do
            -- Create module IR
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.import"
                        , List [Symbol "export", Symbol "value"]
                        , List [Symbol "def", Symbol "value", Integer 123]
                        ]

            -- Build registry
            case buildRegistry [moduleIR] of
                Left err -> expectationFailure $ "Registry build failed: " ++ show err
                Right registry -> do
                    -- Create initial environment with import function
                    let initialEnv = envFromModule builtin

                    -- Create initial eval state with registry
                    let initialState =
                            Runtime
                                { env = initialEnv
                                , context = []
                                , registry = registry
                                , importCache = Cache.emptyCache
                                , rootEnv = initialEnv
                                }

                    -- Evaluate import
                    let importIR = List [Symbol "import", Symbol "test.import"]
                    result <- runEval (eval importIR) initialState

                    case result of
                        Right (Void, finalState) -> do
                            -- Check that the exported symbol is now available
                            case E.lookupVar "value" finalState.env of
                                Right (Integer 123) -> pure ()
                                Right val -> expectationFailure $ "Wrong value imported: " ++ show val
                                Left err -> expectationFailure $ "Symbol not found after import: " ++ show err
                        Right (val, _) -> expectationFailure $ "Import should return Nothing, got: " ++ show val
                        Left err -> expectationFailure $ "Import failed: " ++ show err

        it "does not pollute the current environment during import" $ do
            -- Create module IR that defines some internal variables
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.pollution"
                        , List [Symbol "export", Symbol "public"]
                        , List [Symbol "def", Symbol "internal", Integer 999] -- Internal variable
                        , List [Symbol "def", Symbol "public", Integer 456] -- Exported variable
                        ]

            -- Build registry
            case buildRegistry [moduleIR] of
                Left err -> expectationFailure $ "Registry build failed: " ++ show err
                Right registry -> do
                    -- Create initial environment with some pre-existing variables
                    let baseEnv = envFromModule builtin
                    let initialEnv = E.defineVar "preexisting" (Integer 123) baseEnv

                    -- Create initial eval state with registry
                    let initialState =
                            Runtime
                                { env = initialEnv
                                , context = []
                                , registry = registry
                                , importCache = Cache.emptyCache
                                , rootEnv = initialEnv
                                }

                    -- Evaluate import
                    let importIR = List [Symbol "import", Symbol "test.pollution"]
                    result <- runEval (eval importIR) initialState

                    case result of
                        Right (Void, finalState) -> do
                            -- Check that pre-existing variable is still there
                            case E.lookupVar "preexisting" finalState.env of
                                Right (Integer 123) -> pure ()
                                Right val -> expectationFailure $ "Pre-existing variable changed: " ++ show val
                                Left err -> expectationFailure $ "Pre-existing variable lost: " ++ show err

                            -- Check that exported symbol is available
                            case E.lookupVar "public" finalState.env of
                                Right (Integer 456) -> pure ()
                                Right val -> expectationFailure $ "Wrong exported value: " ++ show val
                                Left err -> expectationFailure $ "Exported symbol not found: " ++ show err

                            -- Check that internal module variable is NOT available (no pollution)
                            case E.lookupVar "internal" finalState.env of
                                Right _ -> expectationFailure "Internal module variable leaked into environment"
                                Left _ -> pure () -- This is expected - internal vars should not be visible
                        Right (val, _) -> expectationFailure $ "Import should return Nothing, got: " ++ show val
                        Left err -> expectationFailure $ "Import failed: " ++ show err

        it "provides both direct and dotted access to imported symbols" $ do
            -- Create module IR
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.dual"
                        , List [Symbol "export", Symbol "add", Symbol "multiply"]
                        , List [Symbol "def", Symbol "add", List [Symbol "+", Integer 1, Integer 2]]
                        , List [Symbol "def", Symbol "multiply", List [Symbol "*", Integer 3, Integer 4]]
                        ]

            -- Build registry
            case buildRegistry [moduleIR] of
                Left err -> expectationFailure $ "Registry build failed: " ++ show err
                Right registry -> do
                    -- Create initial environment with import function
                    let initialEnv = envFromModules [builtin, arithmetic]

                    -- Create initial eval state with registry
                    let initialState =
                            Runtime
                                { env = initialEnv
                                , context = []
                                , registry = registry
                                , importCache = Cache.emptyCache
                                , rootEnv = initialEnv
                                }

                    -- Evaluate import
                    let importIR = List [Symbol "import", Symbol "test.dual"]
                    result <- runEval (eval importIR) initialState

                    case result of
                        Right (Void, finalState) -> do
                            -- Check direct access works
                            case E.lookupVar "add" finalState.env of
                                Right _ -> pure ()
                                Left err -> expectationFailure $ "Direct access failed: " ++ show err

                            -- Check module value is stored
                            case E.lookupVar "test.dual" finalState.env of
                                Right (Module moduleMap) -> do
                                    case Map.lookup "add" moduleMap of
                                        Just _ -> pure ()
                                        Nothing -> expectationFailure "Module missing 'add' property"
                                    case Map.lookup "multiply" moduleMap of
                                        Just _ -> pure ()
                                        Nothing -> expectationFailure "Module missing 'multiply' property"
                                Right val -> expectationFailure $ "Expected Module, got: " ++ show val
                                Left err -> expectationFailure $ "Module not found: " ++ show err
                        Right (val, _) -> expectationFailure $ "Import should return Nothing, got: " ++ show val
                        Left err -> expectationFailure $ "Import failed: " ++ show err

        it "prevents modification of module properties" $ do
            -- Create module IR
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.immutable"
                        , List [Symbol "export", Symbol "value"]
                        , List [Symbol "def", Symbol "value", Integer 42]
                        ]

            -- Build registry
            case buildRegistry [moduleIR] of
                Left err -> expectationFailure $ "Registry build failed: " ++ show err
                Right registry -> do
                    -- Create initial environment with import function
                    let initialEnv = envFromModule builtin

                    -- Create initial eval state with registry
                    let initialState =
                            Runtime
                                { env = initialEnv
                                , context = []
                                , registry = registry
                                , importCache = Cache.emptyCache
                                , rootEnv = initialEnv
                                }

                    -- Evaluate import
                    let importIR = List [Symbol "import", Symbol "test.immutable"]
                    result <- runEval (eval importIR) initialState

                    case result of
                        Right (Void, finalState) -> do
                            -- Try to set module property - should fail
                            let setIR = List [Symbol "set", Symbol "test.immutable.value", Integer 999]
                            setResult <- runEval (eval setIR) finalState

                            case setResult of
                                Left _ -> pure () -- Expected to fail
                                Right _ -> expectationFailure "Setting module property should fail"
                        Right (val, _) -> expectationFailure $ "Import should return Nothing, got: " ++ show val
                        Left err -> expectationFailure $ "Import failed: " ++ show err

        it "allows local definitions to shadow imported symbols" $ do
            -- Create module IR
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.shadow"
                        , List [Symbol "export", Symbol "x"]
                        , List [Symbol "def", Symbol "x", Integer 100]
                        ]

            -- Build registry
            case buildRegistry [moduleIR] of
                Left err -> expectationFailure $ "Registry build failed: " ++ show err
                Right registry -> do
                    -- Create initial environment with import function
                    let initialEnv = envFromModule builtin

                    -- Create initial eval state with registry
                    let initialState =
                            Runtime
                                { env = initialEnv
                                , context = []
                                , registry = registry
                                , importCache = Cache.emptyCache
                                , rootEnv = initialEnv
                                }

                    -- Evaluate import
                    let importIR = List [Symbol "import", Symbol "test.shadow"]
                    result <- runEval (eval importIR) initialState

                    case result of
                        Right (Void, importState) -> do
                            -- Check imported value
                            case E.lookupVar "x" importState.env of
                                Right (Integer 100) -> pure ()
                                Right val -> expectationFailure $ "Wrong imported value: " ++ show val
                                Left err -> expectationFailure $ "Imported value not found: " ++ show err

                            -- Define local x that shadows the imported one
                            let defIR = List [Symbol "def", Symbol "x", Integer 200]
                            defResult <- runEval (eval defIR) importState

                            case defResult of
                                Right (Void, finalState) -> do
                                    -- Check that local definition shadows imported one
                                    case E.lookupVar "x" finalState.env of
                                        Right (Integer 200) -> pure ()
                                        Right val -> expectationFailure $ "Wrong shadowed value: " ++ show val
                                        Left err -> expectationFailure $ "Local value not found: " ++ show err

                                    -- Check that module still has original value
                                    case E.lookupVar "test.shadow" finalState.env of
                                        Right (Module moduleMap) -> do
                                            case Map.lookup "x" moduleMap of
                                                Just (Integer 100) -> pure ()
                                                Just val -> expectationFailure $ "Module value changed: " ++ show val
                                                Nothing -> expectationFailure "Module missing 'x' property"
                                        Right val -> expectationFailure $ "Expected Module, got: " ++ show val
                                        Left err -> expectationFailure $ "Module not found: " ++ show err
                                Right (val, _) -> expectationFailure $ "Def should return Nothing, got: " ++ show val
                                Left err -> expectationFailure $ "Def failed: " ++ show err
                        Right (val, _) -> expectationFailure $ "Import should return Nothing, got: " ++ show val
                        Left err -> expectationFailure $ "Import failed: " ++ show err

        it "supports dotted property access on modules" $ do
            -- Create module IR
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.dotted"
                        , List [Symbol "export", Symbol "value"]
                        , List [Symbol "def", Symbol "value", Integer 42]
                        ]

            -- Build registry
            case buildRegistry [moduleIR] of
                Left err -> expectationFailure $ "Registry build failed: " ++ show err
                Right registry -> do
                    -- Create initial environment with import function
                    let initialEnv = envFromModule builtin

                    -- Create initial eval state with registry
                    let initialState =
                            Runtime
                                { env = initialEnv
                                , context = []
                                , registry = registry
                                , importCache = Cache.emptyCache
                                , rootEnv = initialEnv
                                }

                    -- Evaluate import
                    let importIR = List [Symbol "import", Symbol "test.dotted"]
                    result <- runEval (eval importIR) initialState

                    case result of
                        Right (Void, finalState) -> do
                            -- Test dotted access to module property
                            let accessIR = DottedSymbol ["test", "dotted", "value"]
                            accessResult <- runEval (eval accessIR) finalState

                            case accessResult of
                                Right (Integer 42, _) -> pure ()
                                Right (val, _) -> expectationFailure $ "Wrong value: " ++ show val
                                Left err -> expectationFailure $ "Dotted access failed: " ++ show err
                        Right (val, _) -> expectationFailure $ "Import should return Nothing, got: " ++ show val
                        Left err -> expectationFailure $ "Import failed: " ++ show err
