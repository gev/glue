import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Floor function (rounds down to nearest integer)
/// Mirrors Haskell Glue.Lib.Math.Utility.Floor.floor exactly
final Ir floor = IrNativeFunc(floorImpl);

/// Floor function implementation (rounds down to nearest integer)
/// Mirrors Haskell Glue.Lib.Math.Utility.Floor.floorImpl exactly
Eval<Ir> floorImpl(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrInteger(n)),
        IrFloat(value: final n) => Eval.pure(IrInteger(n.floor())),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
