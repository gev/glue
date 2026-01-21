import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';

/// Equality comparison function
/// Mirrors Haskell Glue.Lib.Bool.Eq.eq exactly
final Ir eq = IrNativeFunc(eqImpl);

/// Equality comparison implementation
/// Mirrors Haskell Glue.Lib.Bool.Eq.eqImpl exactly
Eval<Ir> eqImpl(Ir a) {
  return Eval.pure(IrNativeFunc(eqRight(a)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Bool.Eq.eqRight exactly
Eval<Ir> Function(Ir) eqRight(Ir a) {
  return (Ir b) => Eval.pure(IrBool(a == b));
}
