import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Append function - concatenates two lists
/// Mirrors Haskell Glue.Lib.List.Append.append exactly
Eval<Ir> append(List<Ir> args) {
  return switch (args) {
    [final list1, final list2] =>
      sequenceAll([eval(list1), eval(list2)]).flatMap((evaluated) {
        final val1 = evaluated[0];
        final val2 = evaluated[1];
        if (val1 is IrList && val2 is IrList) {
          return Eval.pure(IrList([...val1.elements, ...val2.elements]));
        } else {
          return throwError(wrongArgumentType(['list', 'list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
