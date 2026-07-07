import 'package:glue/src/env.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/ref.dart';

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
      identical(this, other) ||
      (other is RegisteredModule &&
          other.name == name &&
          _listsEqual(other.exports, exports) &&
          _listsEqual(other.body, body));

  @override
  int get hashCode => Object.hash(name, exports, body);
}

/// A cached imported module with evaluated exports and evaluation stack
class ImportedModule {
  final String moduleName;
  final Map<String, Ref<Ir>> exportedValues;

  const ImportedModule({
    required this.moduleName,
    required this.exportedValues,
  });

  @override
  String toString() =>
      "ImportedModule {moduleName = $moduleName, exports = ${exportedValues.keys.toList()}}";

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      (other is ImportedModule && other.moduleName == moduleName);

  @override
  int get hashCode => moduleName.hashCode;
}

/// Result of parsing a module
class ModuleInfo {
  final String moduleName;
  final List<String> exports;
  final List<(String, Ref<Ir>)> definitions;

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
      identical(this, other) ||
      (other is ModuleInfo &&
          other.moduleName == moduleName &&
          _listsEqual(other.exports, exports) &&
          _listsEqual(other.definitions, definitions));

  @override
  int get hashCode => Object.hash(moduleName, exports, definitions);
}

/// Factory function for native modules
/// Mirrors Haskell nativeModule - exports all definitions by default
ModuleInfo nativeModule(String moduleName, List<(String, Ir)> definitions) =>
    ModuleInfo(
      moduleName: moduleName,
      exports: definitions.map((pair) {
        final (name, _) = pair;
        return name;
      }).toList(),
      definitions: definitions
          .map((entry) => (entry.$1, Ref(entry.$2)))
          .toList(),
    );

/// Create environment from module
Env envFromModule(ModuleInfo module) => fromFrame(frameFromModule(module));

/// Create environment from multiple modules
Env envFromModules(List<ModuleInfo> modules) =>
    fromFrame(unionFrames(modules.map(frameFromModule).toList()));

/// Create frame from module definitions
Frame frameFromModule(ModuleInfo module) => frameFromList(module.definitions);

/// Helper functions for list equality
bool _listsEqual(List a, List b) {
  if (identical(a, b)) return true;
  if (a.length != b.length) return false;
  for (int i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}
