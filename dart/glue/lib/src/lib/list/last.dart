import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Last function - returns the last element of a list
/// Mirrors Haskell Glue.Lib.List.Last.last exactly
Ir last = IrNativeFunc(lastImpl);

/// Last function implementation
/// Mirrors Haskell Glue.Lib.List.Last.lastImpl exactly
Eval<Ir> lastImpl(Ir arg) {
  return eval(arg).flatMap((val) {
    if (val is IrList) {
      if (val.elements.isNotEmpty) {
        return Eval.pure(val.elements.last);
      } else {
        return throwError(wrongArgumentType(['non-empty list']));
      }
    } else {
      return throwError(wrongArgumentType(['list']));
    }
  });
}
