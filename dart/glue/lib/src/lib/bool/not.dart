import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Logical not function
/// Mirrors Haskell Glue.Lib.Bool.Not.not_ exactly
final Ir not = IrNativeFunc(notImpl);

/// Logical not implementation
/// Mirrors Haskell Glue.Lib.Bool.Not.notImpl exactly
Eval<Ir> notImpl(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((value) {
      return switch (value) {
        IrBool(value: false) => Eval.pure(IrBool(true)),
        _ => Eval.pure(IrBool(false)),
      };
    }),
    _ => throwError(wrongArgumentType(['arg'])),
  };
}
