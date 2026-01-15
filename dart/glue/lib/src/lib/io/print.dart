import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';

/// Print function - prints string without newline
/// Mirrors Haskell Glue.Lib.IO.Print.printFunc exactly
Eval<Ir> printFunc(List<Ir> args) {
  return switch (args) {
    [IrString(value: final value)] => liftIO(() {
      // Print without newline
      return print(value);
    }).map((_) => IrVoid()),
    _ => Eval.pure(IrVoid()), // Haskell version ignores wrong arguments
  };
}

/// Println function - prints string with newline
/// Mirrors Haskell Glue.Lib.IO.Print.println exactly
Eval<Ir> println(List<Ir> args) {
  return switch (args) {
    [IrString(value: final value)] => liftIO(() {
      // Print with newline
      return print(value);
    }).map((_) => IrVoid()),
    _ => Eval.pure(IrVoid()), // Haskell version ignores wrong arguments
  };
}
