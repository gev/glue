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
  return (Ir list2) {
    return sequenceAll([eval(list1), eval(list2)]).flatMap((evaluated) {
      return switch (evaluated) {
        [final val1, final val2] =>
          val1 is IrList && val2 is IrList
              ? Eval.pure(IrList([...val1.elements, ...val2.elements]))
              : throwError(wrongArgumentType(['list', 'list'])),
        _ => throwError(wrongArgumentType(['list', 'list'])),
      };
    });
  };
}
