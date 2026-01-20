import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Car function - returns the first element of a list
/// Mirrors Haskell Glue.Lib.List.Car.car exactly
Ir car = IrNativeFunc(carImpl);

/// Car function implementation
/// Mirrors Haskell Glue.Lib.List.Car.carImpl exactly
Eval<Ir> carImpl(Ir arg) {
  return eval(arg).flatMap((val) {
    if (val is IrList) {
      if (val.elements.isNotEmpty) {
        return Eval.pure(val.elements[0]);
      } else {
        return throwError(wrongArgumentType(['non-empty list']));
      }
    } else {
      return throwError(wrongArgumentType(['list']));
    }
  });
}
