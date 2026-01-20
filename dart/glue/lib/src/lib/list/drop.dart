import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Drop function - removes first N elements from a list
/// Mirrors Haskell Glue.Lib.List.Drop.drop exactly
Ir drop = IrNativeFunc(dropImpl);

/// Drop function implementation
/// Mirrors Haskell Glue.Lib.List.Drop.dropImpl exactly
Eval<Ir> dropImpl(Ir countIr) {
  return Eval.pure(IrNativeFunc(dropFrom(countIr)));
}

/// Helper function for list argument
/// Mirrors Haskell Glue.Lib.List.Drop.dropFrom exactly
Eval<Ir> Function(Ir) dropFrom(Ir countIr) {
  return (Ir listIr) {
    return sequenceAll([eval(countIr), eval(listIr)]).flatMap((evaluated) {
      return switch (evaluated) {
        [final count, final list] =>
          count is IrInteger && list is IrList
              ? count.value < 0
                    ? throwError(wrongArgumentType(['non-negative integer']))
                    : () {
                        final dropCount = count.value;
                        final elements = list.elements;
                        final resultElements = dropCount >= elements.length
                            ? <Ir>[]
                            : elements.skip(dropCount).toList();
                        return Eval.pure(IrList(resultElements));
                      }()
              : throwError(wrongArgumentType(['number', 'list'])),
        _ => throwError(wrongArgumentType(['number', 'list'])),
      };
    });
  };
}
