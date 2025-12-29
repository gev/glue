module Reactor.Module.Registration where

import Data.IORef (IORef, modifyIORef, newIORef)
import Data.Map.Strict qualified as Map
import Reactor.Eval (Eval, liftIO, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Module (Module (..), ModuleRegistry)

-- | Global registry reference
type RegistryRef m = IORef (ModuleRegistry m)

-- | Create a new empty registry
newRegistry :: IO (RegistryRef m)
newRegistry = newIORef Map.empty

-- | Module special form - coordinates module registration
moduleForm :: RegistryRef Eval -> [IR Eval] -> Eval (Maybe (IR Eval))
moduleForm registry args = case args of
    [Symbol name, List (List (Symbol "export" : exportSymbols) : bodyForms)] -> do
        -- Validate export symbols
        let validateSymbol (Symbol s) = Just s
            validateSymbol _ = Nothing

        case mapM validateSymbol exportSymbols of
            Just exportNames -> do
                -- Create and register module
                let mod = Module{name, exports = exportNames, body = bodyForms}
                liftIO $ modifyIORef registry (Map.insert name mod)
                pure Nothing
            Nothing -> throwError $ WrongArgumentType ["export symbols"]
    _ -> throwError $ WrongArgumentType ["module name", "(export ...)", "body forms"]

-- | Create registration environment frame
registrationFrame :: RegistryRef Eval -> Frame Eval
registrationFrame registry =
    Map.fromList
        [ ("module", Native (Special (moduleForm registry)))
        ]
