import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Addition function
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Add.add exactly
Eval<Ir> add(List<Ir> args) {
  return switch (args) {
    [final left, final right] => sequenceAll([eval(left), eval(right)]).flatMap(
      (values) {
        final l = values[0];
        final r = values[1];
        return switch ((l, r)) {
          (IrInteger(value: final a), IrInteger(value: final b)) => Eval.pure(
            IrInteger(a + b),
          ),
          (IrInteger(value: final a), IrFloat(value: final b)) => Eval.pure(
            IrFloat(a + b),
          ),
          (IrFloat(value: final a), IrInteger(value: final b)) => Eval.pure(
            IrFloat(a + b),
          ),
          (IrFloat(value: final a), IrFloat(value: final b)) => Eval.pure(
            IrFloat(a + b),
          ),
          _ => throwError(wrongArgumentType(['number'])),
        };
      },
    ),
    _ => throwError(wrongNumberOfArguments()),
  };
}
