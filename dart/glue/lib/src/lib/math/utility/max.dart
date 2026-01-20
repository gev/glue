import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Maximum function (returns the larger of two numbers)
/// Mirrors Haskell Glue.Lib.Math.Utility.Max.max exactly
final Ir max = IrNativeFunc(maxImpl);

/// Maximum function implementation (returns the larger of two numbers)
/// Mirrors Haskell Glue.Lib.Math.Utility.Max.maxImpl exactly
Eval<Ir> maxImpl(Ir left) {
  return Eval.pure(IrNativeFunc(maxWith(left)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Math.Utility.Max.maxWith exactly
Eval<Ir> Function(Ir) maxWith(Ir left) {
  return (Ir right) {
    return sequenceAll([eval(left), eval(right)]).flatMap((values) {
      return switch (values) {
        [IrInteger(value: final n1), IrInteger(value: final n2)] => Eval.pure(
          IrInteger(n1 > n2 ? n1 : n2),
        ),
        [IrFloat(value: final n1), IrFloat(value: final n2)] => Eval.pure(
          IrFloat(n1 > n2 ? n1 : n2),
        ),
        [IrInteger(value: final n1), IrFloat(value: final n2)] => Eval.pure(
          IrFloat(n1 > n2 ? n1.toDouble() : n2),
        ),
        [IrFloat(value: final n1), IrInteger(value: final n2)] => Eval.pure(
          IrFloat(n1 > n2 ? n1 : n2.toDouble()),
        ),
        _ => throwError(wrongArgumentType(['number', 'number'])),
      };
    });
  };
}
