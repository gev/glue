import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/module/cache.dart';
import 'package:glue/src/module/registry.dart';

/// Import special form - loads and evaluates a module
/// Mirrors Haskell Glue.Lib.Builtin.Import.importForm exactly
Eval<Ir> importForm(List<Ir> args) {
  if (args.length != 1) {
    return throwError(wrongArgumentType(['module-name']));
  }

  final moduleNameIr = args[0];
  if (moduleNameIr is! IrSymbol) {
    return throwError(wrongArgumentType(['module-name']));
  }

  final moduleName = moduleNameIr.value;

  return getRegistry().flatMap((registry) {
    final registeredModule = lookupModule(moduleName, registry);
    if (registeredModule == null) {
      return throwError(moduleNotFound(moduleName));
    }

    return getCache().flatMap((cache) {
      final cachedModule = lookupImportedModule(moduleName, cache);

      if (cachedModule != null) {
        // Module already imported - merge into current environment
        return _mergeImportedModule(cachedModule, moduleName);
      } else {
        // First import - evaluate module in isolation
        return _evaluateAndCacheModule(registeredModule, moduleName);
      }
    });
  });
}

/// Merge already cached imported module into current environment
Eval<Ir> _mergeImportedModule(ImportedModule imported, String moduleName) {
  return getEnv().flatMap((currentEnv) {
    // Merge exported values directly into environment
    var updatedEnv = currentEnv;
    for (final entry in imported.exportedValues.entries) {
      updatedEnv = defineVar(entry.key, entry.value, updatedEnv);
    }

    // Store module reference under module name for dotted access
    final moduleIr = IrModule(imported.exportedValues);
    updatedEnv = defineVar(moduleName, moduleIr, updatedEnv);

    return putEnv(updatedEnv).map((_) => IrVoid());
  });
}

/// Evaluate module in isolation and cache the result
Eval<Ir> _evaluateAndCacheModule(
  RegisteredModule registered,
  String moduleName,
) {
  // Get root environment for consistent evaluation
  return getRootEnv().flatMap((rootEnv) {
    // Create isolated environment with just builtins
    final builtinsFrame = rootEnv.isNotEmpty
        ? rootEnv.last
        : IMap<String, Ir>();
    final isolatedEnv = IList<Frame>([builtinsFrame]);

    return getRuntime().flatMap((currentRuntime) {
      // Create isolated runtime
      final isolatedRuntime = currentRuntime.copyWith(env: isolatedEnv);

      // Evaluate module body in isolation
      return liftIO(
        runEval(_evalModuleBody(registered.body), isolatedRuntime),
      ).flatMap((result) {
        return result.match((error) => throwError(error.exception), (success) {
          final (evalResult, finalIsolatedRuntime) = success;

          // Extract exported values from final environment
          final moduleEnv = finalIsolatedRuntime.env;
          final exportedValues = <String, Ir>{};

          for (final exportName in registered.exports) {
            final lookupResult = lookupVar(exportName, moduleEnv);
            lookupResult.match((_) {
              // Exported symbol not found - this is an error in module definition
              throw StateError('Exported symbol not defined: $exportName');
            }, (value) => exportedValues[exportName] = value);
          }

          // Create imported module record
          final importedModule = ImportedModule(
            moduleName: moduleName,
            exportedValues: exportedValues,
            evaluationRootEnv: rootEnv,
          );

          // Cache the imported module
          return getCache().flatMap((cache) {
            final newCache = storeImportedModule(cache, importedModule);
            return putCache(newCache).flatMap((_) {
              // Merge into current environment
              return _mergeImportedModule(importedModule, moduleName);
            });
          });
        });
      });
    });
  });
}

/// Evaluate a list of expressions (module body)
Eval<Ir> _evalModuleBody(List<Ir> body) {
  if (body.isEmpty) {
    return Eval.pure(IrVoid());
  }

  // Evaluate all expressions in sequence, return the last result
  Eval<Ir> evalSequence(List<Ir> exprs) {
    if (exprs.length == 1) {
      return eval(exprs[0]);
    } else {
      return eval(exprs[0]).flatMap((_) => evalSequence(exprs.sublist(1)));
    }
  }

  return evalSequence(body);
}
