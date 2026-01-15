import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Cdr function - returns the rest of a list after the first element
/// Mirrors Haskell Glue.Lib.List.Cdr.cdr exactly
Eval<Ir> cdr(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((val) {
      if (val is IrList) {
        if (val.elements.isNotEmpty) {
          return Eval.pure(IrList(val.elements.sublist(1).toList()));
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
