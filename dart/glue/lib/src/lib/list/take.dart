import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Take function - returns first N elements of a list
/// Mirrors Haskell Glue.Lib.List.Take.take exactly
Ir take = IrNativeFunc(takeImpl);

/// Take function implementation
/// Mirrors Haskell Glue.Lib.List.Take.takeImpl exactly
Eval<Ir> takeImpl(Ir countIr) {
  return Eval.pure(IrNativeFunc(takeFrom(countIr)));
}

/// Helper function for list argument
/// Mirrors Haskell Glue.Lib.List.Take.takeFrom exactly
Eval<Ir> Function(Ir) takeFrom(Ir countIr) {
  return (Ir listIr) {
    return sequenceAll([eval(countIr), eval(listIr)]).flatMap((evaluated) {
      return switch (evaluated) {
        [final count, final list] =>
          count is IrInteger && list is IrList
              ? count.value < 0
                    ? throwError(wrongArgumentType(['non-negative integer']))
                    : () {
                        final takeCount = count.value;
                        final elements = list.elements;
                        final resultElements = takeCount >= elements.length
                            ? elements.toList()
                            : elements.take(takeCount).toList();
                        return Eval.pure(IrList(resultElements));
                      }()
              : throwError(wrongArgumentType(['number', 'list'])),
        _ => throwError(wrongArgumentType(['number', 'list'])),
      };
    });
  };
}
