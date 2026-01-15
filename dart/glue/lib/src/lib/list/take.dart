import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Take function - returns first N elements of a list
/// Mirrors Haskell Glue.Lib.List.Take.take exactly
Eval<Ir> take(List<Ir> args) {
  return switch (args) {
    [final countIr, final listIr] =>
      sequenceAll([eval(countIr), eval(listIr)]).flatMap((evaluated) {
        final count = evaluated[0];
        final list = evaluated[1];
        if (count is IrInteger && list is IrList) {
          if (count.value < 0) {
            return throwError(wrongArgumentType(['non-negative integer']));
          } else {
            final takeCount = count.value;
            final elements = list.elements;
            final resultElements = takeCount >= elements.length
                ? elements.toList()
                : elements.sublist(0, takeCount).toList();
            return Eval.pure(IrList(resultElements));
          }
        } else {
          return throwError(wrongArgumentType(['number', 'list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
