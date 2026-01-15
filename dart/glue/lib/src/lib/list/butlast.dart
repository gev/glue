import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Butlast function - returns all elements except the last one
/// Mirrors Haskell Glue.Lib.List.Butlast.butlast exactly
Eval<Ir> butlast(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((val) {
      if (val is IrList) {
        if (val.elements.isEmpty) {
          return throwError(wrongArgumentType(['non-empty list']));
        } else if (val.elements.length == 1) {
          return Eval.pure(IrList([]));
        } else {
          final resultElements = val.elements
              .sublist(0, val.elements.length - 1)
              .toList();
          return Eval.pure(IrList(resultElements));
        }
      } else {
        return throwError(wrongArgumentType(['list']));
      }
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
