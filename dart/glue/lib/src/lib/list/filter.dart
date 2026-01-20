import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Filter function - filters elements that satisfy a predicate
/// Mirrors Haskell Glue.Lib.List.Filter.filter exactly
Ir filter = IrNativeFunc(filterImpl);

/// Filter function implementation
/// Mirrors Haskell Glue.Lib.List.Filter.filterImpl exactly
Eval<Ir> filterImpl(Ir predicateIr) {
  return Eval.pure(IrNativeFunc(filterList(predicateIr)));
}

/// Helper function for list argument
/// Mirrors Haskell Glue.Lib.List.Filter.filterList exactly
Eval<Ir> Function(Ir) filterList(Ir predicateIr) {
  return (Ir listIr) {
    return sequenceAll([eval(predicateIr), eval(listIr)]).flatMap((evaluated) {
      return switch (evaluated) {
        [final predicate, final list] =>
          list is IrList
              ? filterElements(
                  predicate,
                  list.elements.toList(),
                ).map((filtered) => IrList(filtered))
              : throwError(wrongArgumentType(['function', 'list'])),
        _ => throwError(wrongArgumentType(['function', 'list'])),
      };
    });
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
