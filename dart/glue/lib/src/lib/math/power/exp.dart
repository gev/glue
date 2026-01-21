import 'dart:math' as math;

import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Exponential function (e^x)
/// Mirrors Haskell Glue.Lib.Math.Power.Exp.exp exactly
final Ir exp = IrNativeFunc(expImpl);

/// Exponential function implementation (e^x)
/// Mirrors Haskell Glue.Lib.Math.Power.Exp.expImpl exactly
Eval<Ir> expImpl(Ir arg) {
  return switch (arg) {
    IrInteger(value: final n) => Eval.pure(IrFloat(math.exp(n.toDouble()))),
    IrFloat(value: final n) => Eval.pure(IrFloat(math.exp(n))),
    _ => throwError(wrongArgumentType(['number'])),
  };
}
