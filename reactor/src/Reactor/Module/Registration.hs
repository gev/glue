module Reactor.Module.Registration where

import Control.Monad (forM_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Reactor.Env qualified as E
import Reactor.Eval (Eval, eval, getEnv, liftIO, putEnv, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Module (Module (..), ModuleRegistry)

-- | Global registry reference
type RegistryRef m = IORef (ModuleRegistry m)

-- | Create a new empty registry
newRegistry :: IO (RegistryRef m)
newRegistry = newIORef Map.empty

-- | Registration context for collecting module metadata during evaluation
data RegistrationContext = RegistrationContext
    { moduleName :: Maybe Text
    , exports :: [Text]
    , definitions :: [(Text, IR Eval)]  -- symbol name -> unevaluated form
    , bodyForms :: [IR Eval]
    }

-- | Create empty registration context
emptyContext :: RegistrationContext
emptyContext = RegistrationContext Nothing [] [] []

-- | Context reference for registration
type ContextRef = IORef RegistrationContext

-- | Create new registration context
newContext :: IO ContextRef
newContext = newIORef emptyContext

-- | Module special form - coordinates module registration
moduleForm :: RegistryRef Eval -> ContextRef -> [IR Eval] -> Eval (Maybe (IR Eval))
moduleForm registry ctxRef args = case args of
    [Symbol name] -> do
        -- Start module registration - set the module name
        liftIO $ modifyIORef ctxRef (\ctx -> ctx { moduleName = Just name })
        pure Nothing
    _ -> throwError $ WrongArgumentType ["module-name"]

-- | Export special form - collects symbol names for export
exportForm :: ContextRef -> [IR Eval] -> Eval (Maybe (IR Eval))
exportForm ctxRef args = do
    -- Validate all arguments are symbols
    let validateSymbol (Symbol s) = Just s
        validateSymbol _ = Nothing

    case mapM validateSymbol args of
        Just symbols -> do
            -- Add to exports list
            liftIO $ modifyIORef ctxRef (\ctx -> ctx { exports = ctx.exports ++ symbols })
            pure Nothing
        Nothing -> throwError $ WrongArgumentType ["symbol names"]

-- | Def special form for registration - records symbol without evaluation
defForm :: ContextRef -> [IR Eval] -> Eval (Maybe (IR Eval))
defForm ctxRef [Symbol name, val] = do
    -- Record the definition without evaluating
    liftIO $ modifyIORef ctxRef (\ctx ->
        ctx { definitions = ctx.definitions ++ [(name, val)] })
    pure Nothing
defForm _ _ = throwError $ WrongArgumentType ["symbol", "value"]

-- | Evaluate module body in registration environment
registerModule :: RegistryRef Eval -> ContextRef -> [IR Eval] -> Eval ()
registerModule registry ctxRef bodyForms = do
    -- Create registration environment frame
    let regFrame = registrationFrame registry ctxRef

    -- Save current environment and create registration environment
    currentEnv <- getEnv
    let registrationEnv = regFrame : currentEnv  -- Put registration frame on top

    -- Evaluate each body form in registration environment
    putEnv registrationEnv
    forM_ bodyForms (\form -> eval form >> pure ())

    -- Restore original environment
    putEnv currentEnv

    -- Complete registration
    ctx <- liftIO $ readIORef ctxRef
    case ctx.moduleName of
        Just name -> do
            -- Convert collected definitions to IR forms
            let defForms = map (\(sym, val) -> List [Symbol "def", Symbol sym, val]) ctx.definitions
            let mod = Module
                    { name
                    , exports = ctx.exports
                    , body = defForms ++ ctx.bodyForms  -- Include collected definitions as IR
                    }
            liftIO $ modifyIORef registry (Map.insert name mod)
        Nothing -> throwError $ WrongArgumentType ["module name not set"]

-- | Register a module from IR using evaluation-based registration
registerModuleFromIR :: RegistryRef Eval -> IR Eval -> Eval ()
registerModuleFromIR registry moduleIR = case moduleIR of
    List (Symbol "module" : Symbol name : body) -> do
        -- Create registration context
        ctxRef <- liftIO newContext

        -- Set initial module name
        liftIO $ modifyIORef ctxRef (\ctx -> ctx { moduleName = Just name })

        -- Evaluate body in registration environment
        registerModule registry ctxRef body

    _ -> throwError $ WrongArgumentType ["(module name body...)"]

-- | Create registration environment frame
registrationFrame :: RegistryRef Eval -> ContextRef -> Frame Eval
registrationFrame registry ctxRef =
    Map.fromList
        [ ("module", Native (Special (moduleForm registry ctxRef)))
        , ("export", Native (Special (exportForm ctxRef)))
        , ("def", Native (Special (defForm ctxRef)))
        ]
