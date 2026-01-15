import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Drop function - removes first N elements from a list
/// Mirrors Haskell Glue.Lib.List.Drop.drop exactly
Eval<Ir> drop(List<Ir> args) {
  return switch (args) {
    [final countIr, final listIr] =>
      sequenceAll([eval(countIr), eval(listIr)]).flatMap((evaluated) {
        final count = evaluated[0];
        final list = evaluated[1];
        if (count is IrInteger && list is IrList) {
          if (count.value < 0) {
            return throwError(wrongArgumentType(['non-negative integer']));
          } else {
            final dropCount = count.value;
            final elements = list.elements;
            final resultElements = dropCount >= elements.length
                ? <Ir>[]
                : elements.skip(dropCount).toList();
            return Eval.pure(IrList(resultElements));
          }
        } else {
          return throwError(wrongArgumentType(['number', 'list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
