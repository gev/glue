import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Cons function - constructs a new list by prepending an element to a list
/// Mirrors Haskell Glue.Lib.List.Cons.cons exactly
Eval<Ir> cons(List<Ir> args) {
  return switch (args) {
    [final headArg, final tailArg] =>
      sequenceAll([eval(headArg), eval(tailArg)]).flatMap((evaluated) {
        final headVal = evaluated[0];
        final tailVal = evaluated[1];
        if (tailVal is IrList) {
          return Eval.pure(IrList([headVal, ...tailVal.elements]));
        } else {
          return throwError(wrongArgumentType(['list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
