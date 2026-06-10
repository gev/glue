import 'package:glue/ir.dart';
import 'package:glue/src/module.dart';

/// Module cache for storing evaluated imported modules
/// Mirrors Haskell ImportedModuleCache functionality

typedef ImportedModuleCache = Map<String, ImportedModule>;

/// Create empty module cache
ImportedModuleCache emptyCache() => {};

/// Store an imported module in the cache
void storeImportedModule(ImportedModuleCache cache, ImportedModule imported) {
  final cached = cache[imported.moduleName];
  if (cached == null) {
    cache[imported.moduleName] = imported;
  } else {
    for (final entry in cached.exportedValues.entries) {
      print('1 $entry');
      final ref = imported.exportedValues[entry.key];
      entry.value.value = ref?.value ?? IrVoid();
    }
    for (final entry in imported.exportedValues.entries) {
      final ref = cached.exportedValues[entry.key];
      if (ref == null) {
        print('2 $ref');
        cached.exportedValues[entry.key] = entry.value;
      }
    }
  }
}

/// Lookup an imported module by name
ImportedModule? lookupImportedModule(ImportedModuleCache cache, String name) =>
    cache[name];

/// Check if a module is cached
bool isModuleCached(ImportedModuleCache cache, String name) =>
    cache.containsKey(name);

/// Get all cached module names
List<String> cachedModuleNames(ImportedModuleCache cache) =>
    cache.keys.toList();

/// Get cache size
int cacheSize(ImportedModuleCache cache) => cache.length;
