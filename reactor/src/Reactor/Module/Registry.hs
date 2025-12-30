module Reactor.Module.Registry where

import Data.Map.Strict (Map, empty, size)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Reactor.Module (RegisteredModule)

-- | Global registry of registered modules
type ModuleRegistry ir = Map Text (RegisteredModule ir)

-- | Lookup a module in the registry
lookupModule :: Text -> ModuleRegistry ir -> Maybe (RegisteredModule ir)
lookupModule = Map.lookup

-- | Create an empty registry
emptyRegistry :: ModuleRegistry ir
emptyRegistry = empty

-- | Get the number of modules in the registry
registrySize :: ModuleRegistry ir -> Int
registrySize = size
