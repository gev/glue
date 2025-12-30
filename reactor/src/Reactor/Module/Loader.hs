module Reactor.Module.Loader where

import Control.Monad (forM)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Reactor.Eval (Eval)
import Reactor.IR (IR (..), compile)
import Reactor.Module (RegisteredModule(..))
import Reactor.Module.Error (ModuleRegistryError)
import Reactor.Module.Registration (buildRegistry)
import Reactor.Module.Registry (ModuleRegistry)
import Reactor.Parser (parseReactor)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

-- | Load all .reactor files from a directory and build a module registry
loadModulesFromDirectory :: FilePath -> IO (Either String [RegisteredModule Eval])
loadModulesFromDirectory dir = do
    files <- listDirectory dir
    let reactorFiles = filter (\f -> takeExtension f == ".reactor") files
    moduleIRs <- forM reactorFiles $ \file -> do
        content <- TIO.readFile (dir </> file)
        case parseReactor content of
            Left err -> return $ Left $ "Parse error in " ++ file ++ ": " ++ show err
            Right ast -> return $ Right (compile ast)
    case sequence moduleIRs of
        Left err -> return $ Left err
        Right irs -> case buildRegistry irs of
            Left regErr -> return $ Left $ "Registry error: " ++ show regErr
            Right registry -> return $ Right (Map.elems registry)

-- | Load a single module file
loadModuleFile :: FilePath -> IO (Either String (IR Eval))
loadModuleFile filePath = do
    content <- TIO.readFile filePath
    case parseReactor content of
        Left err -> return $ Left $ "Parse error in " ++ filePath ++ ": " ++ show err
        Right ast -> return $ Right (compile ast)

-- | Build registry from a list of registered modules
buildRegistryFromModules :: [RegisteredModule Eval] -> Either (ModuleRegistryError Eval) (ModuleRegistry Eval)
buildRegistryFromModules modules = buildRegistry (map toIR modules)
  where
    toIR (RegisteredModule {name = n, exports = e, body = b}) = List ([Symbol "module", Symbol n, List (Symbol "export" : map Symbol e)] ++ b)
