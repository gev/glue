import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Addition function
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Add.add exactly
final Ir add = IrNativeFunc(addImpl);

/// Addition function implementation
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Add.addImpl exactly
Eval<Ir> addImpl(Ir left) {
  return Eval.pure(IrNativeFunc(addTo(left)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.Add.addTo exactly
Eval<Ir> Function(Ir) addTo(Ir left) {
  return (Ir right) => switch ((left, right)) {
    (IrInteger(value: final a), IrInteger(value: final b)) => Eval.pure(
      IrInteger(a + b),
    ),
    (IrInteger(value: final a), IrFloat(value: final b)) => Eval.pure(
      IrFloat(a + b),
    ),
    (IrFloat(value: final a), IrInteger(value: final b)) => Eval.pure(
      IrFloat(a + b),
    ),
    (IrFloat(value: final a), IrFloat(value: final b)) => Eval.pure(
      IrFloat(a + b),
    ),
    _ => throwError(wrongArgumentType(['number'])),
  };
}
