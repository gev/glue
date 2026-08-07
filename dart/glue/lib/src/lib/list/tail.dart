import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Tail function - returns the rest of a list after the first element
/// Mirrors Haskell Glue.Lib.List.Tail.tail exactly
Ir tail = IrNativeFunc(tailImpl);

/// Tail function implementation
/// Mirrors Haskell Glue.Lib.List.Tail.tailImpl exactly
Eval<Ir> tailImpl(Ir arg) {
  return switch (arg) {
    IrList(:final elements) =>
      elements.isEmpty
          ? throwError(wrongArgumentType(['non-empty list']))
          : Eval.pure(IrList(elements.skip(1).toList())),
    _ => throwError(wrongArgumentType(['list'])),
  };
}
