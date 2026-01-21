import 'dart:math' as math;

import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Arccosine function (returns radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Acos.acos exactly
final Ir acos = IrNativeFunc(acosImpl);

/// Arccosine function implementation (returns radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Acos.acosImpl exactly
Eval<Ir> acosImpl(Ir arg) {
  return switch (arg) {
    IrInteger(value: final n) => Eval.pure(IrFloat(math.acos(n.toDouble()))),
    IrFloat(value: final n) => Eval.pure(IrFloat(math.acos(n))),
    _ => throwError(wrongArgumentType(['number'])),
  };
}
