module Reactor.Module.Registration where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Reactor.Eval (Eval)
import Reactor.IR (IR (..))
import Reactor.Module (RegisteredModule (..))
import Reactor.Module.Error (ModuleRegistryError (..))
import Reactor.Module.Registry (ModuleRegistry)
import Prelude hiding (mod)

-- | Parse a single module from IR
parseModule :: IR Eval -> Either ModuleRegistryError ModuleInfo
parseModule (List (Symbol "module" : Symbol name : body)) = do
    exports <- extractExports body
    definitions <- extractDefinitions body
    pure $ ModuleInfo name exports definitions
parseModule _ = Left $ InvalidModuleStructure "Expected (module name body...)"

-- | Extract exports from module body
extractExports :: [IR Eval] -> Either ModuleRegistryError [Text]
extractExports [] = pure []
extractExports (List (Symbol "export" : symbols) : rest) = do
    symTexts <- extractSymbolList symbols
    remaining <- extractExports rest
    pure $ symTexts ++ remaining
extractExports (_ : rest) = extractExports rest

-- | Extract definitions from module body
extractDefinitions :: [IR Eval] -> Either ModuleRegistryError [(Text, IR Eval)]
extractDefinitions [] = pure []
extractDefinitions (List [Symbol "def", Symbol name, val] : rest) = do
    remaining <- extractDefinitions rest
    pure $ (name, val) : remaining
extractDefinitions (_ : rest) = extractDefinitions rest

-- | Extract list of symbols
extractSymbolList :: [IR Eval] -> Either ModuleRegistryError [Text]
extractSymbolList [] = pure []
extractSymbolList (Symbol s : rest) = do
    remaining <- extractSymbolList rest
    pure $ s : remaining
extractSymbolList invalid = Left $ InvalidExportList invalid

-- | Build registry from multiple modules
buildRegistry :: [IR Eval] -> Either ModuleRegistryError (ModuleRegistry Eval)
buildRegistry modules = do
    moduleInfos <- mapM parseModule modules
    foldM addModuleToRegistry Map.empty moduleInfos
  where
    addModuleToRegistry reg info
        | Map.member info.moduleName reg = Left $ DuplicateModuleName info.moduleName
        | otherwise = pure $ Map.insert info.moduleName (moduleInfoToModule info) reg

-- | Convert ModuleInfo to Module
moduleInfoToModule :: ModuleInfo -> RegisteredModule Eval
moduleInfoToModule info =
    RegisteredModule
        { name = info.moduleName
        , exports = info.exports
        , body = map (\(sym, val) -> List [Symbol "def", Symbol sym, val]) info.definitions
        }

-- | Result of parsing a module
data ModuleInfo = ModuleInfo
    { moduleName :: Text
    , exports :: [Text]
    , definitions :: [(Text, IR Eval)]
    }
    deriving (Show, Eq)

-- | Register a single module into an existing registry (pure)
registerModule :: ModuleRegistry Eval -> IR Eval -> Either ModuleRegistryError (ModuleRegistry Eval)
registerModule registry moduleIR = do
    moduleInfo <- parseModule moduleIR
    let mod = moduleInfoToModule moduleInfo
    if Map.member moduleInfo.moduleName registry
        then Left $ DuplicateModuleName moduleInfo.moduleName
        else Right $ Map.insert moduleInfo.moduleName mod registry

-- | Register multiple modules into a registry (pure)
registerModules :: ModuleRegistry Eval -> [IR Eval] -> Either ModuleRegistryError (ModuleRegistry Eval)
registerModules = foldM registerModule
