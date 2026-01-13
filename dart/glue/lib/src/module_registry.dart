import 'module.dart';

/// Module registry for storing registered modules
/// Mirrors Haskell module registry functionality

typedef ModuleRegistry = Map<String, RegisteredModule>;

/// Create empty module registry
ModuleRegistry emptyRegistry() => {};

/// Register a module in the registry
/// Returns (errorMessage, newRegistry) - error if duplicate name
(String?, ModuleRegistry?) registerModule(
  ModuleRegistry registry,
  RegisteredModule module,
) {
  if (registry.containsKey(module.name)) {
    return ('Module "${module.name}" already registered', null);
  }
  return (null, {...registry, module.name: module});
}

/// Register multiple modules
/// Returns (errorMessage, newRegistry) - error if any duplicates
(String?, ModuleRegistry?) registerModules(
  ModuleRegistry registry,
  List<RegisteredModule> modules,
) {
  var currentRegistry = registry;
  for (final module in modules) {
    final (error, newRegistry) = registerModule(currentRegistry, module);
    if (error != null) {
      return (error, null);
    }
    currentRegistry = newRegistry!;
  }
  return (null, currentRegistry);
}

/// Lookup a module by name
RegisteredModule? lookupModule(String name, ModuleRegistry registry) =>
    registry[name];

/// Get the number of registered modules
int registrySize(ModuleRegistry registry) => registry.length;

/// Get all registered module names
List<String> registeredModuleNames(ModuleRegistry registry) =>
    registry.keys.toList();
