import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Less than or equal comparison function
/// Mirrors Haskell Glue.Lib.Bool.Le.le exactly
final Ir le = IrNativeFunc(leImpl);

/// Less than or equal comparison implementation
/// Mirrors Haskell Glue.Lib.Bool.Le.leImpl exactly
Eval<Ir> leImpl(List<Ir> args) {
  return switch (args) {
    [final a, final b] => sequenceAll([eval(a), eval(b)]).flatMap((evaluated) {
      final va = evaluated[0];
      final vb = evaluated[1];
      return switch ((va, vb)) {
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
    }),
    _ => throwError(wrongArgumentType(['number', 'number'])),
  };
}
