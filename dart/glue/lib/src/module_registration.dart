import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/module_registry.dart';

/// Module registration system
/// Parses (module name (export ...) body...) IR structures into RegisteredModule instances

/// Parse a module IR structure into a RegisteredModule
/// Returns (errorMessage, registeredModule) - error if parsing fails
(String?, RegisteredModule?) parseModule(Ir ir) {
  // Must be a list starting with 'module'
  if (ir is! IrList || ir.elements.isEmpty) {
    return ('Module declaration must be a non-empty list', null);
  }

  final elements = ir.elements;
  if (elements[0] is! IrSymbol || (elements[0] as IrSymbol).value != 'module') {
    return ('Module declaration must start with "module"', null);
  }

  if (elements.length < 3) {
    return ('Module declaration requires name, exports, and body', null);
  }

  // Extract module name
  final nameIr = elements[1];
  if (nameIr is! IrSymbol) {
    return ('Module name must be a symbol', null);
  }
  final moduleName = (nameIr).value;

  // Extract exports
  final exportsIr = elements[2];
  final (exportError, exports) = _parseExports(exportsIr);
  if (exportError != null) {
    return (exportError, null);
  }

  // Extract body (remaining elements)
  final body = elements.sublist(3).toList();

  return (
    null,
    RegisteredModule(name: moduleName, exports: exports!, body: body),
  );
}

/// Parse export list from (export symbol ...)
/// Returns (errorMessage, exportList) - error if parsing fails
(String?, List<String>?) _parseExports(Ir ir) {
  if (ir is! IrList) {
    return ('Export declaration must be a list', null);
  }

  if (ir.elements.isEmpty) {
    return ('Export declaration cannot be empty', null);
  }

  final first = ir.elements[0];
  if (first is! IrSymbol || first.value != 'export') {
    return ('Export declaration must start with "export"', null);
  }

  final exports = <String>[];
  for (final element in ir.elements.sublist(1)) {
    if (element is! IrSymbol) {
      return ('Export list can only contain symbols', null);
    }
    exports.add(element.value);
  }

  return (null, exports);
}

/// Build registry from multiple module IR structures
/// Returns (errorMessage, registry) - error if any module fails to parse
(String?, ModuleRegistry?) buildRegistry(List<Ir> moduleIRs) {
  var registry = emptyRegistry();
  for (final ir in moduleIRs) {
    final (error, module) = parseModule(ir);
    if (error != null) {
      return (error, null);
    }
    final (regError, newRegistry) = registerModule(registry, module!);
    if (regError != null) {
      return (regError, null);
    }
    registry = newRegistry!;
  }
  return (null, registry);
}

/// Parse and register multiple modules
/// Returns (errorMessage, registry) - error if parsing or registration fails
(String?, ModuleRegistry?) registerModulesFromIR(
  ModuleRegistry registry,
  List<Ir> moduleIRs,
) {
  for (final ir in moduleIRs) {
    final (error, module) = parseModule(ir);
    if (error != null) {
      return (error, null);
    }
    final (regError, newRegistry) = registerModule(registry, module!);
    if (regError != null) {
      return (regError, null);
    }
    registry = newRegistry!;
  }
  return (null, registry);
}
