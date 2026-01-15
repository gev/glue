import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Equality comparison function
/// Mirrors Haskell Glue.Lib.Bool.Eq.eq exactly
Eval<Ir> eq(List<Ir> args) {
  return switch (args) {
    [final a, final b] => sequenceAll([eval(a), eval(b)]).flatMap((evaluated) {
      final va = evaluated[0];
      final vb = evaluated[1];
      return Eval.pure(IrBool(va == vb));
    }),
    _ => throwError(wrongArgumentType(['arg', 'arg'])),
  };
}
