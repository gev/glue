import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

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
