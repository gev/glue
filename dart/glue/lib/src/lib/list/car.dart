import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Car function - returns the first element of a list
/// Mirrors Haskell Glue.Lib.List.Car.car exactly
Eval<Ir> car(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((val) {
      if (val is IrList) {
        if (val.elements.isNotEmpty) {
          return Eval.pure(val.elements[0]);
        } else {
          return throwError(wrongArgumentType(['non-empty list']));
        }
      } else {
        return throwError(wrongArgumentType(['list']));
      }
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
