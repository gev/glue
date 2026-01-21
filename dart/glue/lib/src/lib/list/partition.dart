import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Partition function - splits list into two lists based on predicate
/// Mirrors Haskell Glue.Lib.List.Partition.partition exactly
Ir partition = IrNativeFunc(partitionImpl);

/// Partition function implementation
/// Mirrors Haskell Glue.Lib.List.Partition.partitionImpl exactly
Eval<Ir> partitionImpl(Ir predicateIr) {
  return Eval.pure(IrNativeFunc(partitionList(predicateIr)));
}

/// Helper function for list argument
/// Mirrors Haskell Glue.Lib.List.Partition.partitionList exactly
Eval<Ir> Function(Ir) partitionList(Ir predicate) {
  return (Ir list) {
    return switch (list) {
      IrList(elements: final elements) =>
        partitionElements(predicate, elements.toList()).map((partitioned) {
          final (matching, nonMatching) = partitioned;
          return IrList([IrList(matching), IrList(nonMatching)]);
        }),
      _ => throwError(wrongArgumentType(['function', 'list'])),
    };
  };
}

/// Helper function to partition list based on predicate
Eval<(List<Ir>, List<Ir>)> partitionElements(Ir predicate, List<Ir> elements) {
  if (elements.isEmpty) {
    return Eval.pure(([], []));
  }

  return applyPredicate(predicate, elements[0]).flatMap((satisfies) {
    return partitionElements(predicate, elements.sublist(1)).map((partitioned) {
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
