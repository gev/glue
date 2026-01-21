import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Less than or equal comparison function
/// Mirrors Haskell Glue.Lib.Bool.Le.le exactly
final Ir le = IrNativeFunc(leImpl);

/// Less than or equal comparison implementation
/// Mirrors Haskell Glue.Lib.Bool.Le.leImpl exactly
Eval<Ir> leImpl(Ir a) {
  return Eval.pure(IrNativeFunc(leRight(a)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Bool.Le.leRight exactly
Eval<Ir> Function(Ir) leRight(Ir a) {
  return (Ir b) => switch ((a, b)) {
    (IrInteger(value: final na), IrInteger(value: final nb)) => Eval.pure(
      IrBool(na <= nb),
    ),
    (IrFloat(value: final na), IrFloat(value: final nb)) => Eval.pure(
      IrBool(na <= nb),
    ),
    (IrInteger(value: final na), IrFloat(value: final nb)) => Eval.pure(
      IrBool(na <= nb),
    ),
    (IrFloat(value: final na), IrInteger(value: final nb)) => Eval.pure(
      IrBool(na <= nb),
    ),
    _ => throwError(wrongArgumentType(['number', 'number'])),
  };
}
