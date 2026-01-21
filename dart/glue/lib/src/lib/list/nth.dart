import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Nth function - returns the element at the specified index in a list
/// Mirrors Haskell Glue.Lib.List.Nth.nth exactly
Ir nth = IrNativeFunc(nthImpl);

/// Nth function implementation
/// Mirrors Haskell Glue.Lib.List.Nth.nthImpl exactly
Eval<Ir> nthImpl(Ir indexIr) {
  return Eval.pure(IrNativeFunc(nthFrom(indexIr)));
}

/// Helper function for list argument
/// Mirrors Haskell Glue.Lib.List.Nth.nthFrom exactly
Eval<Ir> Function(Ir) nthFrom(Ir index) {
  return (Ir list) => switch ((index, list)) {
    (IrInteger(:final value), IrList(:final elements)) =>
      (value < 0 || value >= elements.length)
          ? throwError(wrongArgumentType(['valid index']))
          : Eval.pure(elements[value]),
    _ => throwError(wrongArgumentType(['number', 'list'])),
  };
}
