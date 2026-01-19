import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Flatten function - flattens nested lists into a single flat list
/// Mirrors Haskell Glue.Lib.List.Flatten.flatten exactly
Ir flatten = IrNativeFunc(flattenImpl);

/// Flatten function implementation
/// Mirrors Haskell Glue.Lib.List.Flatten.flattenImpl exactly
Eval<Ir> flattenImpl(List<Ir> args) {
  return switch (args) {
    [final listIr] => eval(listIr).flatMap((list) {
      if (list is IrList) {
        return flattenList(
          list.elements.toList(),
        ).map((flattened) => IrList(flattened));
      } else {
        return throwError(wrongArgumentType(['list']));
      }
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}

/// Helper function to flatten a list recursively
Eval<List<Ir>> flattenList(List<Ir> elements) {
  if (elements.isEmpty) {
    return Eval.pure([]);
  }

  final head = elements[0];
  final tail = elements.sublist(1);

  if (head is IrList) {
    // Head is a list, flatten it and concatenate with flattened tail
    return flattenList(head.elements.toList()).flatMap((flattenedHead) {
      return flattenList(
        tail,
      ).map((flattenedTail) => flattenedHead + flattenedTail);
    });
  } else {
    // Head is not a list, keep it and flatten the tail
    return flattenList(tail).map((flattenedTail) => [head, ...flattenedTail]);
  }
}
