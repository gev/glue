import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Greater than comparison function
/// Mirrors Haskell Glue.Lib.Bool.Gt.gt exactly
final Ir gt = IrNativeFunc(gtImpl);

/// Greater than comparison implementation
/// Mirrors Haskell Glue.Lib.Bool.Gt.gtImpl exactly
Eval<Ir> gtImpl(Ir a) {
  return Eval.pure(IrNativeFunc(gtRight(a)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Bool.Gt.gtRight exactly
Eval<Ir> Function(Ir) gtRight(Ir a) {
  return (Ir b) => switch ((a, b)) {
    (IrInteger(value: final na), IrInteger(value: final nb)) => Eval.pure(
      IrBool(na > nb),
    ),
    (IrFloat(value: final na), IrFloat(value: final nb)) => Eval.pure(
      IrBool(na > nb),
    ),
    (IrInteger(value: final na), IrFloat(value: final nb)) => Eval.pure(
      IrBool(na > nb),
    ),
    (IrFloat(value: final na), IrInteger(value: final nb)) => Eval.pure(
      IrBool(na > nb),
    ),
    _ => throwError(wrongArgumentType(['number', 'number'])),
  };
}
