import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Less than comparison function
/// Mirrors Haskell Glue.Lib.Bool.Lt.lt exactly
final Ir lt = IrNativeFunc(ltImpl);

/// Less than comparison implementation
/// Mirrors Haskell Glue.Lib.Bool.Lt.ltImpl exactly
Eval<Ir> ltImpl(Ir a) {
  return Eval.pure(IrNativeFunc(ltRight(a)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Bool.Lt.ltRight exactly
Eval<Ir> Function(Ir) ltRight(Ir a) {
  return (Ir b) {
    return sequenceAll([eval(a), eval(b)]).flatMap((values) {
      return switch (values) {
        [IrInteger(value: final na), IrInteger(value: final nb)] => Eval.pure(
          IrBool(na < nb),
        ),
        [IrFloat(value: final na), IrFloat(value: final nb)] => Eval.pure(
          IrBool(na < nb),
        ),
        [IrInteger(value: final na), IrFloat(value: final nb)] => Eval.pure(
          IrBool(na < nb),
        ),
        [IrFloat(value: final na), IrInteger(value: final nb)] => Eval.pure(
          IrBool(na < nb),
        ),
        _ => throwError(wrongArgumentType(['number', 'number'])),
      };
    });
  };
}
