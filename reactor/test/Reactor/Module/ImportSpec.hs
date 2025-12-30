module Reactor.Module.ImportSpec where

import Data.Map.Strict qualified as Map
import Reactor.Env qualified as E
import Reactor.Eval (Eval, eval, runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Module.Registration (newRegistry, registerModuleFromIR)
import Reactor.Module.System (libWithModules)
import Test.Hspec

spec :: Spec
spec = do
    describe "Module import functionality" $ do
        it "imports module and makes exported symbols available" $ do
            registry <- newRegistry
            let moduleIR =
                    List
                        [ Symbol "module"
                        , Symbol "test.import"
                        , List [Symbol "export", Symbol "value"]
                        , List [Symbol "def", Symbol "value", Number 123]
                        ]

            -- Register the module
            registerModuleFromIR registry moduleIR `shouldReturn` Right ()

            -- Create environment with import function
            let env = E.fromFrame (Map.union lib (libWithModules registry))

            -- Evaluate import
            let importIR = List [Symbol "import", Symbol "test.import"]
            result <- runEval (eval importIR) env

            case result of
                Right (Nothing, finalEnv, _) -> do
                    -- Check that the exported symbol is now available
                    case E.lookupVar "value" finalEnv of
                        Right (Number 123) -> pure ()
                        Right val -> expectationFailure $ "Wrong value imported: " ++ show val
                        Left err -> expectationFailure $ "Symbol not found after import: " ++ show err
                Right (val, _, _) -> expectationFailure $ "Import should return Nothing, got: " ++ show val
                Left err -> expectationFailure $ "Import failed: " ++ show err
