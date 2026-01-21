import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Butlast function - returns all elements except the last one
/// Mirrors Haskell Glue.Lib.List.Butlast.butlast exactly
Ir butlast = IrNativeFunc(butlastImpl);

/// Butlast function implementation
/// Mirrors Haskell Glue.Lib.List.Butlast.butlastImpl exactly
Eval<Ir> butlastImpl(Ir arg) {
  return switch (arg) {
    IrList(elements: final elements) =>
      elements.isEmpty
          ? throwError(wrongArgumentType(['non-empty list']))
          : elements.length == 1
          ? Eval.pure(IrList([]))
          : Eval.pure(
              IrList(elements.sublist(0, elements.length - 1).toList()),
            ),
    _ => throwError(wrongArgumentType(['list'])),
  };
}
