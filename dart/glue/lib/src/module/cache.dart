import 'package:glue/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/ref.dart';

/// Module cache for storing evaluated imported modules
/// Mirrors Haskell ImportedModuleCache functionality

typedef ImportedModuleCache = Map<String, ImportedModule>;

/// Create empty module cache
ImportedModuleCache emptyCache() => {};

/// Store an imported module in the cache
ImportedModule storeImportedModule(
  ImportedModuleCache cache,
  ImportedModule imported,
) {
  final cached = cache[imported.moduleName];
  if (cached == null) {
    cache[imported.moduleName] = imported;
    return imported;
  } else {
    for (final entry in cached.members.entries) {
      final ref = imported.members[entry.key];
      entry.value.value = ref?.value ?? IrVoid();
    }
    for (final entry in imported.members.entries) {
      final ref = cached.members[entry.key];
      if (ref == null) {
        cached.members[entry.key] = entry.value;
      }
      if (imported.exportedValues.containsKey(entry.key)) {
        cached.exportedValues[entry.key] = entry.value;
      } else {
        cached.exportedValues.remove(entry.key);
      }
    }
  }
  return cached;
}

/// Lookup an imported module by name
ImportedModule? lookupImportedModule(ImportedModuleCache cache, String name) =>
    cache[name];

/// Check if the module is cached
bool isModuleCached(ImportedModuleCache cache, String name) =>
    cache.containsKey(name);

/// Get all cached module names
List<String> cachedModuleNames(ImportedModuleCache cache) =>
    cache.keys.toList();

/// Get cache size
int cacheSize(ImportedModuleCache cache) => cache.length;
