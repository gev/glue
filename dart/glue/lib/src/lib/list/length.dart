import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Length function - returns the number of elements in a list
/// Mirrors Haskell Glue.Lib.List.Length.length exactly
Eval<Ir> length(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((val) {
      if (val is IrList) {
        return Eval.pure(IrInteger(val.elements.length));
      } else {
        return throwError(wrongArgumentType(['list']));
      }
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
