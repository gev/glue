import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Nth function - returns the element at the specified index in a list
/// Mirrors Haskell Glue.Lib.List.Nth.nth exactly
Eval<Ir> nth(List<Ir> args) {
  return switch (args) {
    [final indexIr, final listIr] =>
      sequenceAll([eval(indexIr), eval(listIr)]).flatMap((evaluated) {
        final index = evaluated[0];
        final list = evaluated[1];
        if (index is IrInteger && list is IrList) {
          final idx = index.value;
          final elements = list.elements;
          if (idx < 0 || idx >= elements.length) {
            return throwError(wrongArgumentType(['valid index']));
          } else {
            return Eval.pure(elements[idx]);
          }
        } else {
          return throwError(wrongArgumentType(['number', 'list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
