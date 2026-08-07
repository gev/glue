import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Concat function - concatenates two lists
/// Mirrors Haskell Glue.Lib.List.Concat.concat exactly
Ir concat = IrNativeFunc(concatImpl);

/// Concat function implementation
/// Mirrors Haskell Glue.Lib.List.Concat.concatImpl exactly
Eval<Ir> concatImpl(Ir list1) {
  return Eval.pure(IrNativeFunc(concatWith(list1)));
}

/// Helper function for second list argument
/// Mirrors Haskell Glue.Lib.List.Concat.concatWith exactly
Eval<Ir> Function(Ir) concatWith(Ir list1) {
  return (Ir list2) => switch ((list1, list2)) {
    (IrList(elements: final e1), IrList(elements: final e2)) => Eval.pure(
      IrList([...e1, ...e2]),
    ),
    _ => throwError(wrongArgumentType(['list', 'list'])),
  };
}
