import 'package:glue/src/module.dart';

/// Module cache for storing evaluated imported modules
/// Mirrors Haskell ImportedModuleCache functionality

typedef ImportedModuleCache = Map<String, ImportedModule>;

/// Create empty module cache
ImportedModuleCache emptyCache() => {};

/// Store an imported module in the cache
void storeImportedModule(
  ImportedModuleCache cache,
  ImportedModule importedModule,
) {
  cache[importedModule.moduleName] = importedModule;
}

/// Lookup an imported module by name
ImportedModule? lookupImportedModule(ImportedModuleCache cache, String name) =>
    cache[name];

/// Check if a module is cached
bool isModuleCached(ImportedModuleCache cache, String name) =>
    cache.containsKey(name);

/// Remove a module from cache
ImportedModuleCache removeFromCache(ImportedModuleCache cache, String name) {
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
