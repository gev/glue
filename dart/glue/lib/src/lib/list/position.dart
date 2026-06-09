import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Position function - finds index of first element that satisfies a predicate
/// Mirrors Haskell Glue.Lib.List.Position.position exactly
Ir position = IrNativeFunc(positionImpl);

/// Position function implementation
/// Mirrors Haskell Glue.Lib.List.Position.positionImpl exactly
Eval<Ir> positionImpl(Ir predicateIr) {
  return Eval.pure(IrNativeFunc(positionIn(predicateIr)));
}

/// Helper function for list argument
/// Mirrors Haskell Glue.Lib.List.Position.positionIn exactly
Eval<Ir> Function(Ir) positionIn(Ir predicate) {
  return (Ir list) {
    return switch (list) {
      IrList(elements: final elements) => findPosition(
        predicate,
        elements.toList(),
        0,
      ),
      _ => throwError(wrongArgumentType(['function', 'list'])),
    };
  };
}

/// Helper function to find position of first element satisfying predicate
Eval<Ir> findPosition(Ir predicate, List<Ir> elements, int index) {
  if (elements.isEmpty) {
    return throwError(wrongArgumentType(['element satisfying predicate']));
  }

  return applyPredicate(predicate, elements[0]).bind((satisfies) {
    if (satisfies) {
      return Eval.pure(IrInteger(index));
    } else {
      return findPosition(predicate, elements.sublist(1), index + 1);
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
