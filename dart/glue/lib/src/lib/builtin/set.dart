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

  final parts;
  if (target case IrSymbol(value: final name)) {
    parts = name.split('.');
  } else if (target case IrDottedSymbol(parts: final p)) {
    parts = p;
  } else {
    return throwError(wrongArgumentType(['target', 'value']));
  }

  return eval(rawVal).flatMap((val) {
    switch (parts.length) {
      case 1:
        // Simple variable assignment: (set var value)
        return updateVarEval(parts[0], val).map((_) => IrVoid());

      case 2:
        // Object or HostValue property assignment: (set obj.prop value)
        final objName = parts[0];
        final prop = parts[1];

        return getEnv().flatMap((env) {
          final result = lookupVar(objName, env);
          return result.match((err) => throwError(err), (currentObj) {
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
        // Nested property assignment: (set obj.prop.subprop value)
        if (parts.length < 2) {
          return throwError(wrongArgumentType(['target', 'value']));
        }

        final objName = parts[0];
        final propPath = parts.sublist(1);
        final finalProp = propPath.last;
        final intermediatePath = propPath.sublist(0, propPath.length - 1);

        return getEnv().flatMap((env) {
          final result = lookupVar(objName, env);
          return result.match((err) => throwError(err), (currentObj) {
            // Navigate to the object containing the final property
            return _navigateToSetterObject(
              currentObj,
              intermediatePath,
            ).flatMap((targetObj) {
              if (targetObj is IrNativeValue) {
                final hostValue = targetObj.value;
                final setter = hostValue.setters[finalProp];
                if (setter != null) {
                  return setter(val);
                } else {
                  return throwError(propertyNotFound(finalProp));
                }
              } else {
                return throwError(notAnObject(targetObj));
              }
            });
          });
        });
    }
  });
}

/// Navigate to the object that contains the property to be set
/// Similar to _evalNestedAccess but for setting (navigates to parent of final property)
Eval<Ir> _navigateToSetterObject(Ir obj, List<String> remainingParts) {
  if (remainingParts.isEmpty) {
    // We've reached the object that contains the property to set
    return Eval.pure(obj);
  }

  final prop = remainingParts[0];
  final rest = remainingParts.sublist(1);

  return switch (obj) {
    IrObject(properties: final props) =>
      props[prop] != null
          ? _navigateToSetterObject(props[prop]!, rest)
          : throwError(propertyNotFound(prop)),

    IrNativeValue(value: final hostValue) =>
      // Handle property access on host values (FFI)
      hostValue.getters[prop] != null
          ? hostValue.getters[prop]!.flatMap(
              (result) => _navigateToSetterObject(result, rest),
            )
          : throwError(propertyNotFound(prop)),

    _ => throwError(notAnObject(obj)),
  };
}
