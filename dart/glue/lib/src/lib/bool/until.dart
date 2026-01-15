import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Until loop special form
/// Mirrors Haskell Glue.Lib.Bool.Until.until_ exactly
Eval<Ir> until_(List<Ir> args) {
  return switch (args) {
    [final cond, ...final body] => _loopUntil(cond, body),
    _ => throwError(wrongArgumentType(['condition', 'body'])),
  };
}

/// Internal loop function for until
Eval<Ir> _loopUntil(Ir cond, List<Ir> body) {
  return switch (body) {
    [] => eval(cond).flatMap((condVal) {
      return switch (condVal) {
        IrBool(value: false) => _loopUntil(cond, body),
        _ => Eval.pure(IrVoid()),
      };
    }),
    _ => sequence_(
      body.map(eval).toList(),
      eval(cond).flatMap((condVal) {
        return switch (condVal) {
          IrBool(value: false) => _loopUntil(cond, body),
          _ => Eval.pure(IrVoid()),
        };
      }),
    ),
  };
}
