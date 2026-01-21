import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Length function - returns the number of elements in a list
/// Mirrors Haskell Glue.Lib.List.Length.length exactly
Ir length = IrNativeFunc(lengthImpl);

/// Length function implementation
/// Mirrors Haskell Glue.Lib.List.Length.lengthImpl exactly
Eval<Ir> lengthImpl(Ir arg) {
  return switch (arg) {
    IrList(elements: final elements) => Eval.pure(IrInteger(elements.length)),
    _ => throwError(wrongArgumentType(['list'])),
  };
}
