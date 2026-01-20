import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Modulo function
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Mod.mod exactly
final Ir mod = IrNativeFunc(modImpl);

/// Modulo function implementation
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Mod.modImpl exactly
Eval<Ir> modImpl(Ir left) {
  return Eval.pure(IrNativeFunc(modBy(left)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Mod.modBy exactly
Eval<Ir> Function(Ir) modBy(Ir left) {
  return (Ir right) {
    return sequenceAll([eval(left), eval(right)]).flatMap((values) {
      return switch (values) {
        [IrInteger(value: final n1), IrInteger(value: final n2)] =>
          n2 == 0 ? throwError(divByZero()) : Eval.pure(IrInteger(n1 % n2)),
        [IrFloat(value: final n1), IrFloat(value: final n2)] =>
          n2 == 0
              ? throwError(divByZero())
              : Eval.pure(IrFloat((n1.toInt() % n2.toInt()).toDouble())),
        [IrInteger(value: final n1), IrFloat(value: final n2)] =>
          n2 == 0
              ? throwError(divByZero())
              : Eval.pure(IrFloat((n1 % n2.toInt()).toDouble())),
        [IrFloat(value: final n1), IrInteger(value: final n2)] =>
          n2 == 0
              ? throwError(divByZero())
              : Eval.pure(IrFloat((n1.toInt() % n2).toDouble())),
        _ => throwError(wrongArgumentType(['number', 'number'])),
      };
    });
  };
}
