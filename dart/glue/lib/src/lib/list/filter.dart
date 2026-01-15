import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Filter function - filters elements that satisfy a predicate
/// Mirrors Haskell Glue.Lib.List.Filter.filter exactly
Eval<Ir> filter(List<Ir> args) {
  return switch (args) {
    [final predicateIr, final listIr] =>
      sequenceAll([eval(predicateIr), eval(listIr)]).flatMap((evaluated) {
        final predicate = evaluated[0];
        final list = evaluated[1];
        if (list is IrList) {
          return filterElements(
            predicate,
            list.elements.toList(),
          ).map((filtered) => IrList(filtered));
        } else {
          return throwError(wrongArgumentType(['function', 'list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}

/// Helper function to filter elements
Eval<List<Ir>> filterElements(Ir predicate, List<Ir> elements) {
  if (elements.isEmpty) {
    return Eval.pure([]);
  }

  return applyPredicate(predicate, elements[0]).flatMap((satisfies) {
    return filterElements(predicate, elements.sublist(1)).map((rest) {
      return satisfies ? [elements[0], ...rest] : rest;
    });
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
