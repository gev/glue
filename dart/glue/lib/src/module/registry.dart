import 'package:glue/either.dart';
import 'package:glue/src/module.dart';

/// Module registry for storing registered modules
/// Mirrors Haskell module registry functionality

typedef ModuleRegistry = Map<String, RegisteredModule>;

/// Create empty module registry
ModuleRegistry emptyRegistry() => {};

/// Register a module in the registry
Either<String, ModuleRegistry> registerModule(
  ModuleRegistry registry,
  RegisteredModule module,
) {
  if (registry.containsKey(module.name)) {
    return Left('Module "${module.name}" already registered');
  }
  return Right({...registry, module.name: module});
}

/// Reregister a module in the registry
void reregisterModule(ModuleRegistry registry, RegisteredModule module) {
  registry[module.name] = module;
}

/// Register multiple modules
Either<String, ModuleRegistry> registerModules(
  ModuleRegistry registry,
  List<RegisteredModule> modules,
) {
  var currentRegistry = registry;
  for (final module in modules) {
    switch (registerModule(currentRegistry, module)) {
      case Right(value: final newRegistry):
        currentRegistry = newRegistry;
      case Left(value: final err):
        return Left(err);
    }
  }
  return Right(currentRegistry);
}

/// Lookup a module by name
RegisteredModule? lookupModule(String name, ModuleRegistry registry) =>
    registry[name];

/// Get the number of registered modules
int registrySize(ModuleRegistry registry) => registry.length;

/// Check if the module is registered
bool isModuleRegistered(ModuleRegistry registry, String name) =>
    registry.containsKey(name);

/// Get all registered module names
List<String> registeredModuleNames(ModuleRegistry registry) =>
    registry.keys.toList();
