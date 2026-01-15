import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Last function - returns the last element of a list
/// Mirrors Haskell Glue.Lib.List.Last.last exactly
Eval<Ir> last(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((val) {
      if (val is IrList) {
        if (val.elements.isNotEmpty) {
          return Eval.pure(val.elements.last);
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
