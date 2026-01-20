import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Division function
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Div.div exactly
final Ir div = IrNativeFunc(divImpl);

/// Division function implementation
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Div.divImpl exactly
Eval<Ir> divImpl(Ir left) {
  return Eval.pure(IrNativeFunc(divBy(left)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Div.divBy exactly
Eval<Ir> Function(Ir) divBy(Ir left) {
  return (Ir right) {
    return sequenceAll([eval(left), eval(right)]).flatMap((values) {
      return switch (values) {
        [IrInteger(value: final a), IrInteger(value: final b)] => Eval.pure(
          IrFloat(a / b),
        ),
        [IrInteger(value: final a), IrFloat(value: final b)] => Eval.pure(
          IrFloat(a / b),
        ),
        [IrFloat(value: final a), IrInteger(value: final b)] => Eval.pure(
          IrFloat(a / b),
        ),
        [IrFloat(value: final a), IrFloat(value: final b)] => Eval.pure(
          IrFloat(a / b),
        ),
        _ => throwError(wrongArgumentType(['number'])),
      };
    });
  };
}
