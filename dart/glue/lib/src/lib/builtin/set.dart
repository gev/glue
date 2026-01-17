import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Set special form - assigns values to variables or object properties
/// Mirrors Haskell Glue.Lib.Builtin.Set.set exactly
Eval<Ir> set(List<Ir> args) {
  if (args.length != 2) {
    return throwError(wrongArgumentType(['target', 'value']));
  }

  final target = args[0];
  final rawVal = args[1];

  if (target is! IrSymbol) {
    return throwError(wrongArgumentType(['target', 'value']));
  }

  return eval(rawVal).flatMap((val) {
    final name = target.value;
    final parts = name.split('.');

    switch (parts.length) {
      case 1:
        // Simple variable assignment: (set var value)
        return updateVarEval(parts[0], val).map((_) => IrVoid());

      case 2:
        // Object or HostValue property assignment: (set obj.prop value)
        final objName = parts[0];
        final prop = parts[1];

        return getEnv().flatMap((env) {
          final lookupResult = lookupVar(objName, env);
          return lookupResult.match((err) => throwError(err), (currentObj) {
            if (currentObj is IrObject) {
              final newObj = IrObject(
                currentObj.properties.add(prop, val).unlock,
              );
              return updateVarEval(objName, newObj).map((_) => IrVoid());
            } else if (currentObj is IrNativeValue) {
              // Handle HostValue property assignment
              final hostValue = currentObj.value;
              final setter = hostValue.setters[prop];
              if (setter != null) {
                return setter(val);
              } else {
                return throwError(propertyNotFound(prop));
              }
            } else {
              return throwError(notAnObject(currentObj));
            }
          });
        });

      default:
        return throwError(wrongArgumentType(['target', 'value']));
    }
  });
}
