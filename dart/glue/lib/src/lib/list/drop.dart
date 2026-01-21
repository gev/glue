import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Drop function - removes first N elements from a list
/// Mirrors Haskell Glue.Lib.List.Drop.drop exactly
Ir drop = IrNativeFunc(dropImpl);

/// Drop function implementation
/// Mirrors Haskell Glue.Lib.List.Drop.dropImpl exactly
Eval<Ir> dropImpl(Ir countIr) {
  return Eval.pure(IrNativeFunc(dropFrom(countIr)));
}

/// Helper function for list argument
/// Mirrors Haskell Glue.Lib.List.Drop.dropFrom exactly
Eval<Ir> Function(Ir) dropFrom(Ir countIr) {
  return (Ir listIr) => switch ((countIr, listIr)) {
    (IrInteger(value: final n), IrList(elements: final elements)) =>
      n < 0
          ? throwError(wrongArgumentType(['non-negative integer']))
          : Eval.pure(
              IrList(n >= elements.length ? <Ir>[] : elements.skip(n).toList()),
            ),
    _ => throwError(wrongArgumentType(['number', 'list'])),
  };
}
