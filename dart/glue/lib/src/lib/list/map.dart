import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Map function - applies a function to each element of a list
/// Mirrors Haskell Glue.Lib.List.Map.map exactly
Ir map = IrNativeFunc(mapImpl);

/// Map function implementation
/// Mirrors Haskell Glue.Lib.List.Map.mapImpl exactly
Eval<Ir> mapImpl(Ir funcIr) {
  return Eval.pure(IrNativeFunc(mapOver(funcIr)));
}

/// Helper function for list argument
/// Mirrors Haskell Glue.Lib.List.Map.mapOver exactly
Eval<Ir> Function(Ir) mapOver(Ir funcIr) {
  return (Ir listIr) {
    return sequenceAll([eval(funcIr), eval(listIr)]).flatMap((evaluated) {
      return switch (evaluated) {
        [final func, final list] =>
          list is IrList
              ? sequenceAll(
                  list.elements
                      .map((element) => eval(IrList([func, element])))
                      .toList(),
                ).map((results) => IrList(results))
              : throwError(wrongArgumentType(['function', 'list'])),
        _ => throwError(wrongArgumentType(['function', 'list'])),
      };
    });
  };
}
