import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Sort function - sorts elements in a list in ascending order
/// Mirrors Haskell Glue.Lib.List.Sort.sort exactly
Eval<Ir> sort(List<Ir> args) {
  if (args.length != 1) {
    return throwError(wrongNumberOfArguments());
  }

  return eval(args[0]).flatMap((val) {
    if (val is IrList) {
      return sortList(
        val.elements.toList(),
      ).map((sortedElements) => IrList(sortedElements));
    } else {
      return throwError(wrongArgumentType(['list']));
    }
  });
}

/// Helper function to sort a list using merge sort
Eval<List<Ir>> sortList(List<Ir> elements) {
  if (elements.length <= 1) {
    return Eval.pure(elements);
  }

  final mid = elements.length ~/ 2;
  final left = elements.sublist(0, mid);
  final right = elements.sublist(mid);

  return sortList(left).flatMap((sortedLeft) {
    return sortList(right).flatMap((sortedRight) {
      return merge(sortedLeft, sortedRight);
    });
  });
}

/// Helper function to merge two sorted lists
Eval<List<Ir>> merge(List<Ir> left, List<Ir> right) {
  if (left.isEmpty) return Eval.pure(right);
  if (right.isEmpty) return Eval.pure(left);

  return compareIr(left[0], right[0]).flatMap((cmp) {
    if (cmp == -1) {
      // left[0] < right[0]
      return merge(left.sublist(1), right).map((rest) => [left[0], ...rest]);
    } else {
      // left[0] >= right[0]
      return merge(left, right.sublist(1)).map((rest) => [right[0], ...rest]);
    }
  });
}

/// Helper function to compare two IR values
/// Returns: -1 if a < b, 0 if a == b, 1 if a > b
Eval<int> compareIr(Ir a, Ir b) {
  if (a is IrInteger && b is IrInteger) {
    return Eval.pure(a.value.compareTo(b.value));
  } else if (a is IrFloat && b is IrFloat) {
    return Eval.pure(a.value.compareTo(b.value));
  } else if (a is IrInteger && b is IrFloat) {
    return Eval.pure(a.value.toDouble().compareTo(b.value));
  } else if (a is IrFloat && b is IrInteger) {
    return Eval.pure(a.value.compareTo(b.value.toDouble()));
  } else if (a is IrString && b is IrString) {
    return Eval.pure(a.value.compareTo(b.value));
  } else if (a is IrSymbol && b is IrSymbol) {
    return Eval.pure(a.value.compareTo(b.value));
  } else {
    return throwError(
      wrongArgumentType(['comparable values (numbers, strings, or symbols)']),
    );
  }
}
