import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Remove function - removes all occurrences of an item from a list
/// Mirrors Haskell Glue.Lib.List.Remove.remove exactly
Ir remove = IrNativeFunc(removeImpl);

/// Remove function implementation
/// Mirrors Haskell Glue.Lib.List.Remove.removeImpl exactly
Eval<Ir> removeImpl(Ir itemIr) {
  return Eval.pure(IrNativeFunc(removeFrom(itemIr)));
}

Eval<Ir> Function(Ir) removeFrom(Ir item) {
  return (Ir list) => switch (list) {
    IrList(:final elements) => Eval.pure(
      IrList(elements.where((element) => element != item).toList()),
    ),
    _ => throwError(wrongArgumentType(['list'])),
  };
}
