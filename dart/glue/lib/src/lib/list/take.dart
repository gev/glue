import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Take function - returns first N elements of a list
/// Mirrors Haskell Glue.Lib.List.Take.take exactly
Ir take = IrNativeFunc(takeImpl);

/// Take function implementation
/// Mirrors Haskell Glue.Lib.List.Take.takeImpl exactly
Eval<Ir> takeImpl(Ir countIr) {
  return Eval.pure(IrNativeFunc(takeFrom(countIr)));
}

/// Helper function for list argument
/// Mirrors Haskell Glue.Lib.List.Take.takeFrom exactly
Eval<Ir> Function(Ir) takeFrom(Ir count) {
  return (Ir list) => switch ((count, list)) {
    (IrInteger(value: final n), IrList(elements: final elements)) =>
      n < 0
          ? throwError(wrongArgumentType(['non-negative integer']))
          : Eval.pure(
              IrList(
                n >= elements.length
                    ? elements.toList()
                    : elements.take(n).toList(),
              ),
            ),
    _ => throwError(wrongArgumentType(['number', 'list'])),
  };
}
