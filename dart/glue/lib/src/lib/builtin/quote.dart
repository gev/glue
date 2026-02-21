import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Quote special form implementation
/// Mirrors Haskell Glue.Lib.Builtin.Quote exactly

/// Quote special form - returns its argument unevaluated
/// Mirrors Haskell Glue.Lib.Builtin.Quote.quote exactly
final Ir quote = IrSpecial(quoteImpl);

/// Quote special form implementation
/// Mirrors Haskell Glue.Lib.Builtin.Quote.quoteImpl exactly
/// 'quote' prevents evaluation of its argument, returning it as-is
Eval<Ir> quoteImpl(List<Ir> args) {
  return switch (args) {
    // Single argument - return it unevaluated (as-is)
    [final x] => Eval.pure(x),
    // Wrong number of arguments
    _ => throwError(wrongArgumentType(['single argument `quote`'])),
  };
}
