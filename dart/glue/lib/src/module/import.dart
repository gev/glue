import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/module/cache.dart';
import 'package:glue/src/module/registry.dart';
import 'package:glue/src/ref.dart';

Eval<Ir> importModule(String moduleName) {
  return getRegistry().bind((registry) {
    final registeredModule = lookupModule(moduleName, registry);
    if (registeredModule == null) {
      return throwError(moduleNotFound(moduleName));
    }

    return getCache().bind((cache) {
      final cachedModule = lookupImportedModule(cache, moduleName);

      if (cachedModule != null) {
        // Module already imported - merge into current environment
        print('merdge');
        return _mergeImportedModule(cachedModule);
      } else {
        // First import - evaluate module in isolation
        print('cache and merdge');
        return _cacheAndMerdgeImortedModule(registeredModule);
      }
    });
  });
}

/// Evaluate module in isolation and cache the result
Eval<ImportedModule> cacheImportedModule(RegisteredModule registered) {
  // Get root environment for consistent evaluation
  return getRuntime().bind((currentRuntime) {
    // Create isolated runtime
    final isolatedRuntime = currentRuntime.copyWith(
      env: currentRuntime.rootEnv,
    );

    // Evaluate module body in isolation
    return liftIO(
      runEval(_evalModuleBody(registered.body), isolatedRuntime),
    ).bind((result) {
      return result.match((error) => throwError(error.exception), (success) {
        final (evalResult, finalIsolatedRuntime) = success;

        // Extract exported values from final environment
        final moduleFrame = finalIsolatedRuntime.env.last;

        //Collect all exports exist, fail on undefined exports
        // All exports validated, build the map
        final exportedValues = <String, Ref<Ir>>{};
        for (final exportName in registered.exports) {
          final lookupResult = moduleFrame[exportName];
          if (lookupResult != null) {
            exportedValues[exportName] = lookupResult;
          }
        }

        // Create imported module record
        final importedModule = ImportedModule(
          moduleName: registered.name,
          exportedValues: exportedValues,
          members: moduleFrame.unlock,
        );

        // Cache the imported module
        return getCache().bind((cache) {
          final module = storeImportedModule(cache, importedModule);
          return Eval.pure(module);
        });
      });
    });
  });
}

/// Evaluate module in isolation and cache the result
/// and merge cached imported module into current environment
Eval<Ir> _cacheAndMerdgeImortedModule(RegisteredModule registered) =>
    cacheImportedModule(
      registered,
    ).bind((importedModule) => _mergeImportedModule(importedModule));

/// Merge already cached imported module into current environment
Eval<Ir> _mergeImportedModule(ImportedModule imported) {
  return getEnv().bind((currentEnv) {
    // Merge exported values directly into environment
    var updatedEnv = currentEnv;
    for (final entry in imported.exportedValues.entries) {
      updatedEnv = defineRef(entry.key, entry.value, updatedEnv);
    }
    return putEnv(updatedEnv).map((_) => IrVoid());
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
      return eval(exprs[0]).bind((_) => evalSequence(exprs.sublist(1)));
    }
  }

  return evalSequence(body);
}
