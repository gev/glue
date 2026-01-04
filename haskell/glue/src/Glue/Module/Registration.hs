module Glue.Module.Registration where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Glue.IR (IR (..))
import Glue.Module (ModuleInfo (..), RegisteredModule (..))
import Glue.Module.Error (ModuleRegistryError (..))
import Glue.Module.Registry (ModuleRegistry)
import Prelude hiding (mod)

-- | Parse a single module from IR
parseModule :: IR m -> Either (ModuleRegistryError m) (ModuleInfo m)
parseModule (List (Symbol "module" : Symbol name : body)) = do
    exports <- extractExports body
    definitions <- extractDefinitions body
    pure $ ModuleInfo name exports definitions
parseModule _ = Left $ InvalidModuleStructure "Expected (module name body...)"

-- | Extract exports from module body
extractExports :: [IR m] -> Either (ModuleRegistryError m) [Text]
extractExports [] = pure []
extractExports (List (Symbol "export" : symbols) : rest) = do
    symTexts <- extractSymbolList symbols
    remaining <- extractExports rest
    pure $ symTexts ++ remaining
extractExports (_ : rest) = extractExports rest

-- | Extract definitions from module body
extractDefinitions :: [IR m] -> Either (ModuleRegistryError m) [(Text, IR m)]
extractDefinitions [] = pure []
extractDefinitions (List [Symbol "def", Symbol name, val] : rest) = do
    remaining <- extractDefinitions rest
    pure $ (name, val) : remaining
extractDefinitions (_ : rest) = extractDefinitions rest

-- | Extract list of symbols
extractSymbolList :: [IR m] -> Either (ModuleRegistryError m) [Text]
extractSymbolList [] = pure []
extractSymbolList (Symbol s : rest) = do
    remaining <- extractSymbolList rest
    pure $ s : remaining
extractSymbolList invalid = Left $ InvalidExportList invalid

-- | Build registry from multiple modules
buildRegistry :: [IR m] -> Either (ModuleRegistryError m) (ModuleRegistry m)
buildRegistry modules = do
    moduleInfos <- mapM parseModule modules
    foldM addModuleToRegistry Map.empty moduleInfos
  where
    addModuleToRegistry reg info
        | Map.member info.moduleName reg = Left $ DuplicateModuleName info.moduleName
        | otherwise = pure $ Map.insert info.moduleName (moduleInfoToModule info) reg

-- | Convert ModuleInfo to Module
moduleInfoToModule :: ModuleInfo m -> RegisteredModule m
moduleInfoToModule info =
    RegisteredModule
        { name = info.moduleName
        , exports = info.exports
        , body = map (\(sym, val) -> List [Symbol "def", Symbol sym, val]) info.definitions
        }

-- | Register a single module into an existing registry (pure)
registerModule :: ModuleRegistry m -> IR m -> Either (ModuleRegistryError m) (ModuleRegistry m)
registerModule registry moduleIR = do
    moduleInfo <- parseModule moduleIR
    let mod = moduleInfoToModule moduleInfo
    if Map.member moduleInfo.moduleName registry
        then Left $ DuplicateModuleName moduleInfo.moduleName
        else Right $ Map.insert moduleInfo.moduleName mod registry

-- | Register multiple modules into a registry (pure)
registerModules :: ModuleRegistry m -> [IR m] -> Either (ModuleRegistryError m) (ModuleRegistry m)
registerModules = foldM registerModule
