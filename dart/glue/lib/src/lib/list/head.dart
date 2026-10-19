import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Head function - returns the first element of a list
/// Mirrors Haskell Glue.Lib.List.Head.head exactly
Ir head = IrNativeFunc(headImpl);

/// Head function implementation
/// Mirrors Haskell Glue.Lib.List.Head.headImpl exactly
Eval<Ir> headImpl(Ir arg) {
  return switch (arg) {
    IrVoid() => Eval.pure(IrVoid()),
    IrList(:final elements) => Eval.pure(
      elements.isEmpty ? IrVoid() : elements.first,
    ),
    _ => throwError(wrongArgumentType(['list'])),
  };
}
