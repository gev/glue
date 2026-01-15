import 'dart:math' as math;

import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

/// Logarithm with arbitrary base function
/// Mirrors Haskell Glue.Lib.Math.Logarithmic.Log.log exactly
Eval<Ir> log(List<Ir> args) {
  return switch (args) {
    [final arg, final base] => sequenceAll([eval(arg), eval(base)]).flatMap((
      values,
    ) {
      final va = values[0];
      final vb = values[1];
      return switch ((va, vb)) {
        (IrInteger(value: final n), IrInteger(value: final b)) => Eval.pure(
          IrFloat(math.log(n.toDouble()) / math.log(b.toDouble())),
        ),
        (IrInteger(value: final n), IrFloat(value: final b)) => Eval.pure(
          IrFloat(math.log(n.toDouble()) / math.log(b)),
        ),
        (IrFloat(value: final n), IrInteger(value: final b)) => Eval.pure(
          IrFloat(math.log(n) / math.log(b.toDouble())),
        ),
        (IrFloat(value: final n), IrFloat(value: final b)) => Eval.pure(
          IrFloat(math.log(n) / math.log(b)),
        ),
        _ => throwError(wrongArgumentType(['number', 'number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
