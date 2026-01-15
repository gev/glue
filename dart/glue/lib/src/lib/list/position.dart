import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Position function - finds index of first element that satisfies a predicate
/// Mirrors Haskell Glue.Lib.List.Position.position exactly
Eval<Ir> position(List<Ir> args) {
  return switch (args) {
    [final predicateIr, final listIr] =>
      sequenceAll([eval(predicateIr), eval(listIr)]).flatMap((evaluated) {
        final predicate = evaluated[0];
        final list = evaluated[1];
        if (list is IrList) {
          return findPosition(predicate, list.elements.toList(), 0);
        } else {
          return throwError(wrongArgumentType(['function', 'list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}

/// Helper function to find position of first element satisfying predicate
Eval<Ir> findPosition(Ir predicate, List<Ir> elements, int index) {
  if (elements.isEmpty) {
    return throwError(wrongArgumentType(['element satisfying predicate']));
  }

  return applyPredicate(predicate, elements[0]).flatMap((satisfies) {
    if (satisfies) {
      return Eval.pure(IrInteger(index));
    } else {
      return findPosition(predicate, elements.sublist(1), index + 1);
    }
  });
}

/// Helper function to apply predicate to an element
Eval<bool> applyPredicate(Ir predicate, Ir element) {
  return eval(IrList([predicate, element])).flatMap((result) {
    if (result is IrBool) {
      return Eval.pure(result.value);
    } else {
      return throwError(wrongArgumentType(['boolean result from predicate']));
    }
  });
}
