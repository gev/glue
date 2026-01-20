import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Cdr function - returns the rest of a list after the first element
/// Mirrors Haskell Glue.Lib.List.Cdr.cdr exactly
Ir cdr = IrNativeFunc(cdrImpl);

/// Cdr function implementation
/// Mirrors Haskell Glue.Lib.List.Cdr.cdrImpl exactly
Eval<Ir> cdrImpl(Ir arg) {
  return eval(arg).flatMap((val) {
    if (val is IrList) {
      if (val.elements.isNotEmpty) {
        return Eval.pure(IrList(val.elements.sublist(1).toList()));
      } else {
        return Eval.pure(IrList([]));
      }
    } else {
      return throwError(wrongArgumentType(['list']));
    }
  });
}
