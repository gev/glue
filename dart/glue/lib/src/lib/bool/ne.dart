import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Not equal comparison function
/// Mirrors Haskell Glue.Lib.Bool.Ne.ne exactly
final Ir ne = IrNativeFunc(neImpl);

/// Not equal comparison implementation
/// Mirrors Haskell Glue.Lib.Bool.Ne.neImpl exactly
Eval<Ir> neImpl(Ir a) {
  return Eval.pure(IrNativeFunc(neRight(a)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Bool.Ne.neRight exactly
Eval<Ir> Function(Ir) neRight(Ir a) {
  return (Ir b) {
    return sequenceAll([eval(a), eval(b)]).flatMap((values) {
      return switch (values) {
        [final va, final vb] => Eval.pure(IrBool(va != vb)),
        _ => throwError(wrongArgumentType(['arg', 'arg'])),
      };
    });
  };
}
