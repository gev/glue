import 'dart:math' as math;

import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Power function (base^exponent)
/// Mirrors Haskell Glue.Lib.Math.Power.Pow.pow exactly
final Ir pow = IrNativeFunc(powImpl);

/// Power function implementation (base^exponent)
/// Mirrors Haskell Glue.Lib.Math.Power.Pow.powImpl exactly
Eval<Ir> powImpl(Ir base) {
  return Eval.pure(IrNativeFunc(powTo(base)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Math.Power.Pow.powTo exactly
Eval<Ir> Function(Ir) powTo(Ir base) {
  return (Ir exponent) {
    return sequenceAll([eval(base), eval(exponent)]).flatMap((values) {
      return switch (values) {
        [IrInteger(value: final b), IrInteger(value: final e)] => Eval.pure(
          IrInteger(math.pow(b, e).toInt()),
        ),
        [IrInteger(value: final b), IrFloat(value: final e)] => Eval.pure(
          IrFloat(math.pow(b.toDouble(), e).toDouble()),
        ),
        [IrFloat(value: final b), IrInteger(value: final e)] => Eval.pure(
          IrFloat(math.pow(b, e.toDouble()).toDouble()),
        ),
        [IrFloat(value: final b), IrFloat(value: final e)] => Eval.pure(
          IrFloat(math.pow(b, e).toDouble()),
        ),
        _ => throwError(wrongArgumentType(['number', 'number'])),
      };
    });
  };
}
