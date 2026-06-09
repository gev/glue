import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Error special form - throws a runtime exception
/// Mirrors Haskell Glue.Lib.Builtin.Error.errorFunc exactly
final Ir error = IrSpecial(errorImpl);

/// Error special form implementation
/// Mirrors Haskell Glue.Lib.Builtin.Error.errorFunc exactly
Eval<Ir> errorImpl(List<Ir> args) {
  if (args.length != 2) {
    return throwError(wrongArgumentType(['symbol', 'value']));
  }

  final name = args[0];
  final rawVal = args[1];

  if (name is! IrSymbol) {
    return throwError(wrongArgumentType(['symbol', 'value']));
  }

  return eval(rawVal).bind((val) {
    return throwError(runtimeException(name.value, val));
  });
}
