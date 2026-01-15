import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Member function - checks if an item is in a list
/// Mirrors Haskell Glue.Lib.List.Member.member exactly
Eval<Ir> member(List<Ir> args) {
  return switch (args) {
    [final itemIr, final listIr] =>
      sequenceAll([eval(itemIr), eval(listIr)]).flatMap((evaluated) {
        final item = evaluated[0];
        final list = evaluated[1];
        if (list is IrList) {
          final isMember = list.elements.contains(item);
          return Eval.pure(IrBool(isMember));
        } else {
          return throwError(wrongArgumentType(['list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
