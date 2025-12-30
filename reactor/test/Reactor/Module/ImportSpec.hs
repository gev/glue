module Reactor.Module.ImportSpec where

import Reactor.Env qualified as E
import Reactor.Eval (EvalState (..), eval, runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Module.Cache qualified as Cache
import Reactor.Module.Registration (buildRegistry)
import Reactor.Module.System (libWithModules)
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
                        , List [Symbol "def", Symbol "value", Number 123]
                        ]

            -- Build registry
            case buildRegistry [moduleIR] of
                Left err -> expectationFailure $ "Registry build failed: " ++ show err
                Right registry -> do
                    -- Create initial environment with import function
                    let initialEnv = E.fromFrame (E.unionFrames lib libWithModules)

                    -- Create initial eval state with registry
                    let initialState =
                            EvalState
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
                        Right (Nothing, finalState) -> do
                            -- Check that the exported symbol is now available
                            case E.lookupVar "value" finalState.env of
                                Right (Number 123) -> pure ()
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
                        , List [Symbol "def", Symbol "internal", Number 999] -- Internal variable
                        , List [Symbol "def", Symbol "public", Number 456] -- Exported variable
                        ]

            -- Build registry
            case buildRegistry [moduleIR] of
                Left err -> expectationFailure $ "Registry build failed: " ++ show err
                Right registry -> do
                    -- Create initial environment with some pre-existing variables
                    let baseEnv = E.fromFrame (E.unionFrames lib libWithModules)
                    let initialEnv = E.defineVar "preexisting" (Number 123) baseEnv

                    -- Create initial eval state with registry
                    let initialState =
                            EvalState
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
                        Right (Nothing, finalState) -> do
                            -- Check that pre-existing variable is still there
                            case E.lookupVar "preexisting" finalState.env of
                                Right (Number 123) -> pure ()
                                Right val -> expectationFailure $ "Pre-existing variable changed: " ++ show val
                                Left err -> expectationFailure $ "Pre-existing variable lost: " ++ show err

                            -- Check that exported symbol is available
                            case E.lookupVar "public" finalState.env of
                                Right (Number 456) -> pure ()
                                Right val -> expectationFailure $ "Wrong exported value: " ++ show val
                                Left err -> expectationFailure $ "Exported symbol not found: " ++ show err

                            -- Check that internal module variable is NOT available (no pollution)
                            case E.lookupVar "internal" finalState.env of
                                Right _ -> expectationFailure "Internal module variable leaked into environment"
                                Left _ -> pure () -- This is expected - internal vars should not be visible
                        Right (val, _) -> expectationFailure $ "Import should return Nothing, got: " ++ show val
                        Left err -> expectationFailure $ "Import failed: " ++ show err
