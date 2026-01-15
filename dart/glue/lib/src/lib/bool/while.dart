import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// While loop special form
/// Mirrors Haskell Glue.Lib.Bool.While.while_ exactly
Eval<Ir> while_(List<Ir> args) {
  return switch (args) {
    [final cond, ...final body] => _loop(cond, body),
    _ => throwError(wrongArgumentType(['condition', 'body'])),
  };
}

/// Internal loop function for while
Eval<Ir> _loop(Ir cond, List<Ir> body) {
  return eval(cond).flatMap((condVal) {
    return switch (condVal) {
      IrBool(value: false) => Eval.pure(IrVoid()),
      _ => switch (body) {
        [] => _loop(cond, body), // Continue loop if no body
        _ => sequence_(body.map(eval).toList(), _loop(cond, body)),
      },
    };
  });
}
