import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

/// Absolute value function
/// Mirrors Haskell Glue.Lib.Math.Utility.Abs.abs exactly
Eval<Ir> abs(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrInteger(n.abs())),
        IrFloat(value: final n) => Eval.pure(IrFloat(n.abs())),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
