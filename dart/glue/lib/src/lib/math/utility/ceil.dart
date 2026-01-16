import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

/// Ceiling function (rounds up to nearest integer)
/// Mirrors Haskell Glue.Lib.Math.Utility.Ceil.ceil exactly
Eval<Ir> ceil(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrInteger(n)),
        IrFloat(value: final n) => Eval.pure(IrInteger(n.ceil())),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
