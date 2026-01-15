import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Less than comparison function
/// Mirrors Haskell Glue.Lib.Bool.Lt.lt exactly
Eval<Ir> lt(List<Ir> args) {
  return switch (args) {
    [final a, final b] => sequenceAll([eval(a), eval(b)]).flatMap((evaluated) {
      final va = evaluated[0];
      final vb = evaluated[1];
      return switch ((va, vb)) {
        (IrInteger(value: final na), IrInteger(value: final nb)) => Eval.pure(
          IrBool(na < nb),
        ),
        (IrFloat(value: final na), IrFloat(value: final nb)) => Eval.pure(
          IrBool(na < nb),
        ),
        (IrInteger(value: final na), IrFloat(value: final nb)) => Eval.pure(
          IrBool(na < nb),
        ),
        (IrFloat(value: final na), IrInteger(value: final nb)) => Eval.pure(
          IrBool(na < nb),
        ),
        _ => throwError(wrongArgumentType(['number', 'number'])),
      };
    }),
    _ => throwError(wrongArgumentType(['number', 'number'])),
  };
}
