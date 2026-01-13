import 'ir.dart' hide Env, Frame;
import 'env.dart';

/// Module system data structures
/// Mirrors Haskell Glue.Module exactly

/// A registered module containing metadata and body for evaluation
class RegisteredModule {
  final String name;
  final List<String> exports;
  final List<Ir> body;

  const RegisteredModule({
    required this.name,
    required this.exports,
    required this.body,
  });

  @override
  String toString() =>
      "Module {name = $name, exports = $exports, body = <${body.length} forms>}";

  @override
  bool operator ==(Object other) =>
      other is RegisteredModule &&
      other.name == name &&
      _listsEqual(other.exports, exports) &&
      _listsEqualIr(other.body, body);

  @override
  int get hashCode => Object.hash(name, exports, body);
}

/// A cached imported module with evaluated exports and evaluation context
class ImportedModule {
  final String moduleName;
  final Map<String, Ir> exportedValues;
  final Env evaluationRootEnv;

  const ImportedModule({
    required this.moduleName,
    required this.exportedValues,
    required this.evaluationRootEnv,
  });

  @override
  String toString() =>
      "ImportedModule {moduleName = $moduleName, exports = ${exportedValues.keys.toList()}}";

  @override
  bool operator ==(Object other) =>
      other is ImportedModule && other.moduleName == moduleName;

  @override
  int get hashCode => moduleName.hashCode;
}

/// Result of parsing a module
class ModuleInfo {
  final String moduleName;
  final List<String> exports;
  final List<(String, Ir)> definitions;

  const ModuleInfo({
    required this.moduleName,
    required this.exports,
    required this.definitions,
  });

  @override
  String toString() =>
      "ModuleInfo {moduleName = $moduleName, exports = $exports, definitions = ${definitions.length}}";

  @override
  bool operator ==(Object other) =>
      other is ModuleInfo &&
      other.moduleName == moduleName &&
      _listsEqual(other.exports, exports) &&
      _listsEqualPairs(other.definitions, definitions);

  @override
  int get hashCode => Object.hash(moduleName, exports, definitions);
}

/// Factory function for native modules
/// Mirrors Haskell nativeModule - exports all definitions by default
ModuleInfo nativeModule(String moduleName, List<(String, Ir)> definitions) =>
    ModuleInfo(
      moduleName: moduleName,
      exports: definitions.map((pair) => pair.$1).toList(),
      definitions: definitions,
    );

/// Create environment from module
Env envFromModule(ModuleInfo module) => fromFrame(frameFromModule(module));

/// Create environment from multiple modules
Env envFromModules(List<ModuleInfo> modules) =>
    fromFrame(unionFrames(modules.map(frameFromModule).toList()));

/// Create frame from module definitions
Frame frameFromModule(ModuleInfo module) => frameFromList(module.definitions);

/// Helper functions for list equality
bool _listsEqual(List<String> a, List<String> b) {
  if (a.length != b.length) return false;
  for (int i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}

bool _listsEqualIr(List<Ir> a, List<Ir> b) {
  if (a.length != b.length) return false;
  for (int i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}

bool _listsEqualPairs(List<(String, Ir)> a, List<(String, Ir)> b) {
  if (a.length != b.length) return false;
  for (int i = 0; i < a.length; i++) {
    if (a[i].$1 != b[i].$1 || a[i].$2 != b[i].$2) return false;
  }
  return true;
}
