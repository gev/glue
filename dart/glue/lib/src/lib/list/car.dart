import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Car function - returns the first element of a list
/// Mirrors Haskell Glue.Lib.List.Car.car exactly
Ir car = IrNativeFunc(carImpl);

/// Car function implementation
/// Mirrors Haskell Glue.Lib.List.Car.carImpl exactly
Eval<Ir> carImpl(Ir arg) {
  return switch (arg) {
    IrList(:final elements) =>
      elements.isEmpty
          ? throwError(wrongArgumentType(['non-empty list']))
          : Eval.pure(elements.first),
    _ => throwError(wrongArgumentType(['list'])),
  };
}
