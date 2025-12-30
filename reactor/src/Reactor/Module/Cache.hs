module Reactor.Module.Cache where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Reactor.Module (ImportedModule (..), ImportedModuleCache)

-- | Lookup a cached imported module
lookupCachedModule :: Text -> ImportedModuleCache m -> Maybe (ImportedModule m)
lookupCachedModule = Map.lookup

-- | Check if a module is already cached
isModuleCached :: Text -> ImportedModuleCache m -> Bool
isModuleCached moduleName cache = Map.member moduleName cache

-- | Add a module to the cache (pure)
cacheModule :: ImportedModule m -> ImportedModuleCache m -> ImportedModuleCache m
cacheModule imported cache = Map.insert (moduleName imported) imported cache

-- | Remove a module from the cache (pure)
uncacheModule :: Text -> ImportedModuleCache m -> ImportedModuleCache m
uncacheModule moduleName cache = Map.delete moduleName cache

-- | Get all cached module names
cachedModuleNames :: ImportedModuleCache m -> [Text]
cachedModuleNames = Map.keys

-- | Create an empty cache
emptyCache :: ImportedModuleCache m
emptyCache = Map.empty

-- | Get cache size
cacheSize :: ImportedModuleCache m -> Int
cacheSize = Map.size

-- | Check if cache is empty
isCacheEmpty :: ImportedModuleCache m -> Bool
isCacheEmpty = Map.null
