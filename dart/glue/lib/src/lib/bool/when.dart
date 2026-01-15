import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// When conditional execution special form
/// Mirrors Haskell Glue.Lib.Bool.When.when_ exactly
Eval<Ir> when_(List<Ir> args) {
  return switch (args) {
    [final cond, ...final body] => eval(cond).flatMap((condVal) {
      return switch (condVal) {
        IrBool(value: false) => Eval.pure(IrVoid()),
        _ => switch (body) {
          [] => Eval.pure(IrVoid()),
          _ => sequenceAll(
            body.map(eval).toList(),
          ).map((results) => results.last),
        },
      };
    }),
    _ => throwError(wrongArgumentType(['condition', 'body'])),
  };
}
