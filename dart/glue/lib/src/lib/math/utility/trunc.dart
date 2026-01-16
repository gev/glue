import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

/// Truncate function (removes decimal part)
/// Mirrors Haskell Glue.Lib.Math.Utility.Trunc.trunc exactly
Eval<Ir> trunc(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrInteger(n)),
        IrFloat(value: final n) => Eval.pure(IrInteger(n.truncate())),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
