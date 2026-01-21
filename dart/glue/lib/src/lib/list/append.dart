import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Append function - concatenates two lists
/// Mirrors Haskell Glue.Lib.List.Append.append exactly
Ir append = IrNativeFunc(appendImpl);

/// Append function implementation
/// Mirrors Haskell Glue.Lib.List.Append.appendImpl exactly
Eval<Ir> appendImpl(Ir list1) {
  return Eval.pure(IrNativeFunc(appendWith(list1)));
}

/// Helper function for second list argument
/// Mirrors Haskell Glue.Lib.List.Append.appendWith exactly
Eval<Ir> Function(Ir) appendWith(Ir list1) {
  return (Ir list2) => switch ((list1, list2)) {
    (IrList(elements: final e1), IrList(elements: final e2)) => Eval.pure(
      IrList([...e1, ...e2]),
    ),
    _ => throwError(wrongArgumentType(['list', 'list'])),
  };
}
