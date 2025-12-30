module Reactor.Module.ImportSpec where

import Data.Map.Strict qualified as Map
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
                    let initialEnv = E.fromFrame (Map.union lib libWithModules)

                    -- Create initial eval state with registry
                    let initialState =
                            EvalState
                                { env = initialEnv
                                , context = []
                                , registry = registry
                                , importCache = Cache.emptyCache
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
