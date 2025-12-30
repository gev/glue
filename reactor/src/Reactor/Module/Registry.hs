module Reactor.Module.Registry where

import Data.Map.Strict (Map, empty, size)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Reactor.Module (RegisteredModule)

-- | Global registry of registered modules
type ModuleRegistry m = Map Text (RegisteredModule m)

-- | Lookup a module in the registry
lookupModule :: Text -> ModuleRegistry m -> Maybe (RegisteredModule m)
lookupModule = Map.lookup

-- | Create an empty registry
emptyRegistry :: ModuleRegistry m
emptyRegistry = empty

-- | Get the number of modules in the registry
registrySize :: ModuleRegistry m -> Int
registrySize = size
