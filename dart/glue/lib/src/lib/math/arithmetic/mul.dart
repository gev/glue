import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Multiplication function
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Mul.mul exactly
final Ir mul = IrNativeFunc(mulImpl);

/// Multiplication function implementation
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Mul.mulImpl exactly
Eval<Ir> mulImpl(Ir left) {
  return Eval.pure(IrNativeFunc(mulTo(left)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Mul.mulTo exactly
Eval<Ir> Function(Ir) mulTo(Ir left) {
  return (Ir right) {
    return sequenceAll([eval(left), eval(right)]).flatMap((values) {
      return switch (values) {
        [IrInteger(value: final a), IrInteger(value: final b)] => Eval.pure(
          IrInteger(a * b),
        ),
        [IrInteger(value: final a), IrFloat(value: final b)] => Eval.pure(
          IrFloat(a * b),
        ),
        [IrFloat(value: final a), IrInteger(value: final b)] => Eval.pure(
          IrFloat(a * b),
        ),
        [IrFloat(value: final a), IrFloat(value: final b)] => Eval.pure(
          IrFloat(a * b),
        ),
        _ => throwError(wrongArgumentType(['number'])),
      };
    });
  };
}
