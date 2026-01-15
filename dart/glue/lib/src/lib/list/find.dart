import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Find function - finds first element that satisfies a predicate
/// Mirrors Haskell Glue.Lib.List.Find.find exactly
Eval<Ir> find(List<Ir> args) {
  return switch (args) {
    [final predicateIr, final listIr] =>
      sequenceAll([eval(predicateIr), eval(listIr)]).flatMap((evaluated) {
        final predicate = evaluated[0];
        final list = evaluated[1];
        if (list is IrList) {
          return findElement(predicate, list.elements.toList());
        } else {
          return throwError(wrongArgumentType(['function', 'list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}

/// Helper function to find first element satisfying predicate
Eval<Ir> findElement(Ir predicate, List<Ir> elements) {
  if (elements.isEmpty) {
    return throwError(wrongArgumentType(['element satisfying predicate']));
  }

  return applyPredicate(predicate, elements[0]).flatMap((satisfies) {
    if (satisfies) {
      return Eval.pure(elements[0]);
    } else {
      return findElement(predicate, elements.sublist(1));
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
