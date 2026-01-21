import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Greater than or equal comparison function
/// Mirrors Haskell Glue.Lib.Bool.Ge.ge exactly
final Ir ge = IrNativeFunc(geImpl);

/// Greater than or equal comparison implementation
/// Mirrors Haskell Glue.Lib.Bool.Ge.geImpl exactly
Eval<Ir> geImpl(Ir a) {
  return Eval.pure(IrNativeFunc(geRight(a)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Bool.Ge.geRight exactly
Eval<Ir> Function(Ir) geRight(Ir a) {
  return (Ir b) => switch ((a, b)) {
    (IrInteger(value: final na), IrInteger(value: final nb)) => Eval.pure(
      IrBool(na >= nb),
    ),
    (IrFloat(value: final na), IrFloat(value: final nb)) => Eval.pure(
      IrBool(na >= nb),
    ),
    (IrInteger(value: final na), IrFloat(value: final nb)) => Eval.pure(
      IrBool(na >= nb),
    ),
    (IrFloat(value: final na), IrInteger(value: final nb)) => Eval.pure(
      IrBool(na >= nb),
    ),
    _ => throwError(wrongArgumentType(['number', 'number'])),
  };
}
