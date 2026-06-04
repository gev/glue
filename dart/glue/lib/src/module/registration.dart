import 'package:glue/either.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/module/registry.dart';

/// Module registration system
/// Parses (module name (export ...) body...) IR structures into RegisteredModule instances

/// Parse a module IR structure into a RegisteredModule
Either<String, RegisteredModule> parseModule(Ir ir) {
  // Must be a list starting with 'module'
  if (ir is! IrList || ir.elements.isEmpty) {
    return Left('Module declaration must be a non-empty list');
  }

  final elements = ir.elements;
  if (elements[0] is! IrSymbol || (elements[0] as IrSymbol).value != 'module') {
    return Left('Module declaration must start with "module"');
  }

  if (elements.length < 3) {
    return Left('Module declaration requires name, exports, and body');
  }

  // Extract module name
  final nameIr = elements[1];
  if (nameIr is! IrSymbol) {
    return Left('Module name must be a symbol');
  }
  final moduleName = nameIr.value;

  // Extract exports
  final exportsIr = elements[2];

  switch (_parseExports(exportsIr)) {
    case Right(value: final exports):
      {
        final body = elements.sublist(3).toList();
        return Right(
          RegisteredModule(name: moduleName, exports: exports, body: body),
        );
      }
    case Left(value: final err):
      return Left(err);
  }
}

/// Parse export list from (export symbol ...)
Either<String, List<String>> _parseExports(Ir ir) {
  if (ir is! IrList) {
    return Left('Export declaration must be a list');
  }

  if (ir.elements.isEmpty) {
    return Left('Export declaration cannot be empty');
  }

  final first = ir.elements[0];
  if (first is! IrSymbol || first.value != 'export') {
    return Left('Export declaration must start with "export"');
  }

  final exports = <String>[];
  for (final element in ir.elements.sublist(1)) {
    if (element is! IrSymbol) {
      return Left('Export list can only contain symbols');
    }
    exports.add(element.value);
  }

  return Right(exports);
}

/// Build registry from multiple module IR structures
Either<String, ModuleRegistry> buildRegistry(List<Ir> moduleIRs) {
  var registry = emptyRegistry();
  for (final ir in moduleIRs) {
    switch (parseModule(ir)) {
      case Right(value: final module):
        {
          switch (registerModule(registry, module)) {
            case Right(value: final newRegistry):
              registry = newRegistry;
            case Left(value: final err):
              return Left(err);
          }
        }
      case Left(value: final err):
        return Left(err);
    }
  }
  return Right(registry);
}

/// Parse and register multiple modules
Either<String, ModuleRegistry> registerModulesFromIR(
  ModuleRegistry registry,
  List<Ir> moduleIRs,
) {
  var currentRegistry = registry;
  for (final ir in moduleIRs) {
    switch (parseModule(ir)) {
      case Right(value: final module):
        {
          switch (registerModule(currentRegistry, module)) {
            case Right(value: final newRegistry):
              currentRegistry = newRegistry;
            case Left(value: final err):
              return Left(err);
          }
        }
      case Left(value: final err):
        return Left(err);
    }
  }
  return Right(currentRegistry);
}
