import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Partition function - splits list into two lists based on predicate
/// Mirrors Haskell Glue.Lib.List.Partition.partition exactly
Eval<Ir> partition(List<Ir> args) {
  return switch (args) {
    [final predicateIr, final listIr] =>
      sequenceAll([eval(predicateIr), eval(listIr)]).flatMap((evaluated) {
        final predicate = evaluated[0];
        final list = evaluated[1];
        if (list is IrList) {
          return partitionList(predicate, list.elements.toList()).map((
            partitioned,
          ) {
            final (matching, nonMatching) = partitioned;
            return IrList([IrList(matching), IrList(nonMatching)]);
          });
        } else {
          return throwError(wrongArgumentType(['function', 'list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}

/// Helper function to partition list based on predicate
Eval<(List<Ir>, List<Ir>)> partitionList(Ir predicate, List<Ir> elements) {
  if (elements.isEmpty) {
    return Eval.pure(([], []));
  }

  return applyPredicate(predicate, elements[0]).flatMap((satisfies) {
    return partitionList(predicate, elements.sublist(1)).map((partitioned) {
      final (matching, nonMatching) = partitioned;
      if (satisfies) {
        return ([elements[0], ...matching], nonMatching);
      } else {
        return (matching, [elements[0], ...nonMatching]);
      }
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
