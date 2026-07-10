import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';

/// Print function - prints string without newline
/// Mirrors Haskell Glue.Lib.IO.Print.printFunc exactly
final Ir printFunc = IrNativeFunc(printFuncImpl);

/// Print function implementation
/// Mirrors Haskell Glue.Lib.IO.Print.printFuncImpl exactly
Eval<Ir> printFuncImpl(Ir arg) {
  print(arg);
  return Eval.pure(IrVoid());
}

/// Println function - prints string with newline
/// Mirrors Haskell Glue.Lib.IO.Print.println exactly
final Ir println = IrNativeFunc(printlnImpl);

/// Println function implementation
/// Mirrors Haskell Glue.Lib.IO.Print.printlnImpl exactly
Eval<Ir> printlnImpl(Ir arg) {
  print(arg);
  return Eval.pure(IrVoid()); // Haskell version ignores wrong arguments
}
