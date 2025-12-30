module Reactor.Module.Cache where

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Reactor.Module (ImportedModule (..))

-- | Global cache of imported modules (evaluated results)
type ImportedModuleCache m = Map Text (ImportedModule m)

-- | Lookup a cached imported module
lookupCachedModule :: Text -> ImportedModuleCache m -> Maybe (ImportedModule m)
lookupCachedModule = Map.lookup

-- | Add a module to the cache (pure)
cacheModule :: ImportedModule m -> ImportedModuleCache m -> ImportedModuleCache m
cacheModule imported = Map.insert (moduleName imported) imported

-- | Create an empty cache
emptyCache :: ImportedModuleCache m
emptyCache = Map.empty
