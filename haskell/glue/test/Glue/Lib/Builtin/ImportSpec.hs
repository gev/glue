module Glue.Lib.Builtin.ImportSpec where

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
        it "imports simple module and makes exported symbols available directly" $ do
            -- Create module IR
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "math"
                        , List [Symbol "export", Symbol "pi"]
                        , List [Symbol "def", Symbol "pi", Integer 314]
                        ]

            -- Build registry
            case buildRegistry [moduleIR] of
                Left err -> expectationFailure $ "Registry build failed: " ++ show err
                Right registry -> do
                    -- Create initial environment with import function
                    let initialEnv = envFromModule builtin

                    -- Create initial eval runtime with registry
                    let initialState =
                            Runtime
                                { env = initialEnv
                                , context = []
                                , registry = registry
                                , importCache = Cache.emptyCache
                                , rootEnv = initialEnv
                                }

                    -- Evaluate import
                    let importIR = List [Symbol "import", Symbol "math"]
                    result <- runEval (eval importIR) initialState

                    case result of
                        Right (Void, finalState) -> do
                            -- Check that the exported symbol is available directly in environment
                            case E.lookupVar "pi" finalState.env of
                                Right (Integer 314) -> pure ()
                                Right val -> expectationFailure $ "Wrong value imported: " ++ show val
                                Left err -> expectationFailure $ "Direct access failed: " ++ show err
                        Right (val, _) -> expectationFailure $ "Import should return Void, got: " ++ show val
                        Left err -> expectationFailure $ "Import failed: " ++ show err

        it "does not pollute the current environment during import" $ do
            -- Create module IR that defines some internal variables
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "utils"
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

                    -- Create initial eval runtime with registry
                    let initialState =
                            Runtime
                                { env = initialEnv
                                , context = []
                                , registry = registry
                                , importCache = Cache.emptyCache
                                , rootEnv = initialEnv
                                }

                    -- Evaluate import
                    let importIR = List [Symbol "import", Symbol "utils"]
                    result <- runEval (eval importIR) initialState

                    case result of
                        Right (Void, finalState) -> do
                            -- Check that pre-existing variable is still there
                            case E.lookupVar "preexisting" finalState.env of
                                Right (Integer 123) -> pure ()
                                Right val -> expectationFailure $ "Pre-existing variable changed: " ++ show val
                                Left err -> expectationFailure $ "Pre-existing variable lost: " ++ show err

                            -- Check that exported symbol is available directly
                            case E.lookupVar "public" finalState.env of
                                Right (Integer 456) -> pure ()
                                Right val -> expectationFailure $ "Wrong exported value: " ++ show val
                                Left err -> expectationFailure $ "Exported symbol not found: " ++ show err

                            -- Check that internal module variable is NOT available (no pollution)
                            case E.lookupVar "internal" finalState.env of
                                Right _ -> expectationFailure "Internal module variable leaked into environment"
                                Left _ -> pure () -- This is expected - internal vars should not be visible
                        Right (val, _) -> expectationFailure $ "Import should return Void, got: " ++ show val
                        Left err -> expectationFailure $ "Import failed: " ++ show err

        it "imports dotted module and makes exported symbols available directly" $ do
            -- Create module IR with dotted name
            let moduleIR =
                    List
                        [ Symbol "module"
                        , DottedSymbol ["test", "dual"]
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

                    -- Create initial eval runtime with registry
                    let initialState =
                            Runtime
                                { env = initialEnv
                                , context = []
                                , registry = registry
                                , importCache = Cache.emptyCache
                                , rootEnv = initialEnv
                                }

                    -- Evaluate import
                    let importIR = List [Symbol "import", DottedSymbol ["test", "dual"]]
                    result <- runEval (eval importIR) initialState

                    case result of
                        Right (Void, finalState) -> do
                            -- Check direct access to exported symbols
                            case E.lookupVar "add" finalState.env of
                                Right _ -> pure ()
                                Left err -> expectationFailure $ "Direct access to add failed: " ++ show err
                            case E.lookupVar "multiply" finalState.env of
                                Right _ -> pure ()
                                Left err -> expectationFailure $ "Direct access to multiply failed: " ++ show err
                        Right (val, _) -> expectationFailure $ "Import should return Void, got: " ++ show val
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

                    -- Create initial eval runtime with registry
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
                                Right (val, _) -> expectationFailure $ "Def should return Void, got: " ++ show val
                                Left err -> expectationFailure $ "Def failed: " ++ show err
                        Right (val, _) -> expectationFailure $ "Import should return Void, got: " ++ show val
                        Left err -> expectationFailure $ "Import failed: " ++ show err

        it "imports multiple modules and makes all exports available directly" $ do
            -- Create multiple module IRs
            let modulesIR =
                    [ List [Symbol "module", Symbol "math", List [Symbol "export", Symbol "pi"], List [Symbol "def", Symbol "pi", Integer 314]]
                    , List [Symbol "module", Symbol "utils", List [Symbol "export", Symbol "len"], List [Symbol "def", Symbol "len", Integer 42]]
                    ]

            -- Build registry
            case buildRegistry modulesIR of
                Left err -> expectationFailure $ "Registry build failed: " ++ show err
                Right registry -> do
                    -- Create initial environment
                    let initialEnv = envFromModule builtin
                    let initialState = Runtime initialEnv [] registry Cache.emptyCache initialEnv

                    -- Import math
                    let importMath = List [Symbol "import", Symbol "math"]
                    result1 <- runEval (eval importMath) initialState
                    state1 <- case result1 of
                        Right (Void, s) -> pure s
                        _ -> expectationFailure "Import math failed" >> undefined

                    -- Import utils
                    let importUtils = List [Symbol "import", Symbol "utils"]
                    result2 <- runEval (eval importUtils) state1
                    finalState <- case result2 of
                        Right (Void, s) -> pure s
                        _ -> expectationFailure "Import utils failed" >> undefined

                    -- Check direct access to all exports
                    case E.lookupVar "pi" finalState.env of
                        Right (Integer 314) -> pure ()
                        _ -> expectationFailure "Direct access to pi failed"
                    case E.lookupVar "len" finalState.env of
                        Right (Integer 42) -> pure ()
                        _ -> expectationFailure "Direct access to len failed"
