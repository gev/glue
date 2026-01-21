import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Cdr function - returns the rest of a list after the first element
/// Mirrors Haskell Glue.Lib.List.Cdr.cdr exactly
Ir cdr = IrNativeFunc(cdrImpl);

/// Cdr function implementation
/// Mirrors Haskell Glue.Lib.List.Cdr.cdrImpl exactly
Eval<Ir> cdrImpl(Ir arg) {
  return switch (arg) {
    IrList(:final elements) =>
      elements.isEmpty
          ? throwError(wrongArgumentType(['non-empty list']))
          : Eval.pure(IrList(elements.skip(1).toList())),
    _ => throwError(wrongArgumentType(['list'])),
  };
}
