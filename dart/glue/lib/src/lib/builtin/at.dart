import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/src/ir.dart';

final Ir at = IrNativeFunc((Ir target) {
  return Eval.pure(
    IrNativeFunc((Ir accessor) {
      switch (target) {
        case IrString(:final value) when accessor is IrInteger:
          final index = accessor.value;
          if (index >= 0 && index < value.length) {
            return Eval.pure(IrString(value[index]));
          }
          return Eval.pure(IrVoid());

        case IrList(:final elements) when accessor is IrInteger:
          final index = accessor.value;
          if (index >= 0 && index < elements.length) {
            return Eval.pure(elements[index]);
          }
          return Eval.pure(IrVoid());

        case IrObject(:final properties) when accessor is IrString:
          return Eval.pure(properties[accessor.value] ?? IrVoid());

        case IrObject(:final properties) when accessor is IrSymbol:
          return Eval.pure(properties[accessor.value] ?? IrVoid());

        case IrObject(:final properties) when accessor is IrDottedSymbol:
          return _nestedLookup(accessor.parts, properties.unlock);

        case IrNativeValue(:final value) when accessor is IrString:
          return value.getters[accessor.value] ?? Eval.pure(IrVoid());

        case IrNativeValue(:final value) when accessor is IrSymbol:
          return value.getters[accessor.value] ?? Eval.pure(IrVoid());

        case IrNativeValue(:final value) when accessor is IrDottedSymbol:
          return _nestedNativeLookup(accessor.parts, value.getters);

        default:
          return throwError(wrongArgumentType(['target', 'accessor']));
      }
    }),
  );
});

Eval<Ir> _nestedLookup(List<String> fields, Map<String, Ir> obj) =>
    switch (fields) {
      [] => Eval.pure(IrVoid()),
      [final key] => Eval.pure(obj[key] ?? IrVoid()),
      [final key, ...final rest] => switch (obj[key]) {
        IrObject(:final properties) => _nestedLookup(rest, properties.unlock),
        _ => Eval.pure(IrVoid()),
      },
    };

Eval<Ir> _nestedNativeLookup(
  List<String> fields,
  Map<String, Eval<Ir>> getters,
) => switch (fields) {
  [] => Eval.pure(IrVoid()),
  [final key] => getters[key] ?? Eval.pure(IrVoid()),
  [final key, ...final rest] => switch (getters[key]) {
    null => Eval.pure(IrVoid()),
    final evalAction => evalAction.bind(
      (resolvedIR) => switch (resolvedIR) {
        IrObject(:final properties) => _nestedLookup(rest, properties.unlock),
        IrNativeValue(:final value) => _nestedNativeLookup(rest, value.getters),
        _ => Eval.pure(IrVoid()),
      },
    ),
  },
};
