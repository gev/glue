import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Not equal comparison function
/// Mirrors Haskell Glue.Lib.Bool.Ne.ne exactly
Eval<Ir> ne(List<Ir> args) {
  return switch (args) {
    [final a, final b] => sequenceAll([eval(a), eval(b)]).flatMap((evaluated) {
      final va = evaluated[0];
      final vb = evaluated[1];
      return Eval.pure(IrBool(va != vb));
    }),
    _ => throwError(wrongArgumentType(['arg', 'arg'])),
  };
}
