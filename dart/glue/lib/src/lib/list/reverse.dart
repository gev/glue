import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Reverse function - reverses the order of elements in a list
/// Mirrors Haskell Glue.Lib.List.Reverse.reverse exactly
Ir reverse = IrNativeFunc(reverseImpl);

/// Reverse function implementation
/// Mirrors Haskell Glue.Lib.List.Reverse.reverseImpl exactly
Eval<Ir> reverseImpl(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((val) {
      if (val is IrList) {
        final reversedElements = val.elements.reversed.toList();
        return Eval.pure(IrList(reversedElements));
      } else {
        return throwError(wrongArgumentType(['list']));
      }
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
