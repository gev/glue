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

Eval<Ir> Function(Ir) removeFrom(Ir itemIr) {
  return (Ir listIr) {
    return sequenceAll([eval(itemIr), eval(listIr)]).flatMap((evaluated) {
      return switch (evaluated) {
        [final item, final list] =>
          list is IrList
              ? Eval.pure(
                  IrList(
                    list.elements.where((element) => element != item).toList(),
                  ),
                )
              : throwError(wrongArgumentType(['list'])),
        _ => throwError(wrongArgumentType(['list'])),
      };
    });
  };
}
