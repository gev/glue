import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Absolute value function
/// Mirrors Haskell Glue.Lib.Math.Utility.Abs.abs exactly
final Ir abs = IrNativeFunc(absImpl);

/// Absolute value function implementation
/// Mirrors Haskell Glue.Lib.Math.Utility.Abs.absImpl exactly
Eval<Ir> absImpl(Ir arg) {
  return switch (arg) {
    IrInteger(value: final n) => Eval.pure(IrInteger(n.abs())),
    IrFloat(value: final n) => Eval.pure(IrFloat(n.abs())),
    _ => throwError(wrongArgumentType(['number'])),
  };
}
