import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Reverse function - reverses the order of elements in a list
/// Mirrors Haskell Glue.Lib.List.Reverse.reverse exactly
Eval<Ir> reverse(List<Ir> args) {
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
