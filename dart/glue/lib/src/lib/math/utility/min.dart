import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Minimum function (returns the smaller of two numbers)
/// Mirrors Haskell Glue.Lib.Math.Utility.Min.min exactly
final Ir min = IrNativeFunc(minImpl);

/// Minimum function implementation (returns the smaller of two numbers)
/// Mirrors Haskell Glue.Lib.Math.Utility.Min.minImpl exactly
Eval<Ir> minImpl(Ir left) {
  return Eval.pure(IrNativeFunc(minWith(left)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Math.Utility.Min.minWith exactly
Eval<Ir> Function(Ir) minWith(Ir left) {
  return (Ir right) => switch ((left, right)) {
    (IrInteger(value: final n1), IrInteger(value: final n2)) => Eval.pure(
      IrInteger(n1 < n2 ? n1 : n2),
    ),
    (IrFloat(value: final n1), IrFloat(value: final n2)) => Eval.pure(
      IrFloat(n1 < n2 ? n1 : n2),
    ),
    (IrInteger(value: final n1), IrFloat(value: final n2)) => Eval.pure(
      IrFloat(n1 < n2 ? n1.toDouble() : n2),
    ),
    (IrFloat(value: final n1), IrInteger(value: final n2)) => Eval.pure(
      IrFloat(n1 < n2 ? n1 : n2.toDouble()),
    ),
    _ => throwError(wrongArgumentType(['number', 'number'])),
  };
}
