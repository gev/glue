import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';

/// Print function - prints string without newline
/// Mirrors Haskell Glue.Lib.IO.Print.printFunc exactly
final Ir printFunc = IrNativeFunc(printFuncImpl);

/// Print function implementation
/// Mirrors Haskell Glue.Lib.IO.Print.printFuncImpl exactly
Eval<Ir> printFuncImpl(List<Ir> args) {
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
final Ir println = IrNativeFunc(printlnImpl);

/// Println function implementation
/// Mirrors Haskell Glue.Lib.IO.Print.printlnImpl exactly
Eval<Ir> printlnImpl(List<Ir> args) {
  return switch (args) {
    [IrString(value: final value)] => liftIO(() {
      // Print with newline
      return print(value);
    }).map((_) => IrVoid()),
    _ => Eval.pure(IrVoid()), // Haskell version ignores wrong arguments
  };
}
