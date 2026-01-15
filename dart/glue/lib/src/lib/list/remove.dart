import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Remove function - removes all occurrences of an item from a list
/// Mirrors Haskell Glue.Lib.List.Remove.remove exactly
Eval<Ir> remove(List<Ir> args) {
  return switch (args) {
    [final itemIr, final listIr] =>
      sequenceAll([eval(itemIr), eval(listIr)]).flatMap((evaluated) {
        final item = evaluated[0];
        final list = evaluated[1];
        if (list is IrList) {
          final filtered = list.elements
              .where((element) => element != item)
              .toList();
          return Eval.pure(IrList(filtered));
        } else {
          return throwError(wrongArgumentType(['list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
