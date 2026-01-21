import 'dart:math' as math;

import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Square root function
/// Mirrors Haskell Glue.Lib.Math.Power.Sqrt.sqrt exactly
final Ir sqrt = IrNativeFunc(sqrtImpl);

/// Square root function implementation
/// Mirrors Haskell Glue.Lib.Math.Power.Sqrt.sqrtImpl exactly
Eval<Ir> sqrtImpl(Ir arg) {
  return switch (arg) {
    IrInteger(value: final n) => Eval.pure(IrFloat(math.sqrt(n.toDouble()))),
    IrFloat(value: final n) => Eval.pure(IrFloat(math.sqrt(n))),
    _ => throwError(wrongArgumentType(['number'])),
  };
}
