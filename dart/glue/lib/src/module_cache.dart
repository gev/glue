import 'module.dart';
import 'env.dart';

/// Module cache for storing evaluated imported modules
/// Mirrors Haskell ImportedModuleCache functionality

typedef ImportedModuleCache = Map<String, ImportedModule>;

/// Create empty module cache
ImportedModuleCache emptyCache() => {};

/// Store an imported module in the cache
ImportedModuleCache storeImportedModule(
  ImportedModuleCache cache,
  ImportedModule importedModule,
) => {...cache, importedModule.moduleName: importedModule};

/// Lookup an imported module by name
ImportedModule? lookupImportedModule(String name, ImportedModuleCache cache) =>
    cache[name];

/// Check if a module is cached
bool isModuleCached(String name, ImportedModuleCache cache) =>
    cache.containsKey(name);

/// Remove a module from cache
ImportedModuleCache removeFromCache(String name, ImportedModuleCache cache) {
  final newCache = {...cache};
  newCache.remove(name);
  return newCache;
}

/// Get all cached module names
List<String> cachedModuleNames(ImportedModuleCache cache) =>
    cache.keys.toList();

/// Get cache size
int cacheSize(ImportedModuleCache cache) => cache.length;

/// Clear entire cache
ImportedModuleCache clearCache() => emptyCache();
