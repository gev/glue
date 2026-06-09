import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Find function - finds first element that satisfies a predicate
/// Mirrors Haskell Glue.Lib.List.Find.find exactly
Ir find = IrNativeFunc(findImpl);

/// Find function implementation
/// Mirrors Haskell Glue.Lib.List.Find.findImpl exactly
Eval<Ir> findImpl(Ir predicateIr) {
  return Eval.pure(IrNativeFunc(findIn(predicateIr)));
}

/// Helper function for list argument
/// Mirrors Haskell Glue.Lib.List.Find.findIn exactly
Eval<Ir> Function(Ir) findIn(Ir predicate) {
  return (Ir list) {
    return switch (list) {
      IrList(elements: final elements) => findElement(
        predicate,
        elements.toList(),
      ),
      _ => throwError(wrongArgumentType(['function', 'list'])),
    };
  };
}

/// Helper function to find first element satisfying predicate
Eval<Ir> findElement(Ir predicate, List<Ir> elements) {
  if (elements.isEmpty) {
    return throwError(wrongArgumentType(['element satisfying predicate']));
  }

  return applyPredicate(predicate, elements[0]).bind((satisfies) {
    if (satisfies) {
      return Eval.pure(elements[0]);
    } else {
      return findElement(predicate, elements.sublist(1));
    }
  });
}

/// Helper function to apply predicate to an element
Eval<bool> applyPredicate(Ir predicate, Ir element) {
  return eval(IrList([predicate, element])).bind((result) {
    if (result is IrBool) {
      return Eval.pure(result.value);
    } else {
      return throwError(wrongArgumentType(['boolean result from predicate']));
    }
  });
}
