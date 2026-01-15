import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Map function - applies a function to each element of a list
/// Mirrors Haskell Glue.Lib.List.Map.map exactly
Eval<Ir> map(List<Ir> args) {
  return switch (args) {
    [final funcIr, final listIr] =>
      sequenceAll([eval(funcIr), eval(listIr)]).flatMap((evaluated) {
        final func = evaluated[0];
        final list = evaluated[1];
        if (list is IrList) {
          // Apply the function to each element by evaluating [func, element]
          final elementEvals = list.elements.map(
            (element) => eval(IrList([func, element])),
          );
          return sequenceAll(
            elementEvals.toList(),
          ).map((results) => IrList(results));
        } else {
          return throwError(wrongArgumentType(['function', 'list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
