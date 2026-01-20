import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Subtraction function
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Sub.sub exactly
final Ir sub = IrNativeFunc(subImpl);

/// Subtraction function implementation
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Sub.subImpl exactly
Eval<Ir> subImpl(Ir left) {
  return Eval.pure(IrNativeFunc(subFrom(left)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Sub.subFrom exactly
Eval<Ir> Function(Ir) subFrom(Ir left) {
  return (Ir right) {
    return sequenceAll([eval(left), eval(right)]).flatMap((values) {
      return switch (values) {
        [IrInteger(value: final a), IrInteger(value: final b)] => Eval.pure(
          IrInteger(a - b),
        ),
        [IrInteger(value: final a), IrFloat(value: final b)] => Eval.pure(
          IrFloat(a - b),
        ),
        [IrFloat(value: final a), IrInteger(value: final b)] => Eval.pure(
          IrFloat(a - b),
        ),
        [IrFloat(value: final a), IrFloat(value: final b)] => Eval.pure(
          IrFloat(a - b),
        ),
        _ => throwError(wrongArgumentType(['number'])),
      };
    });
  };
}
