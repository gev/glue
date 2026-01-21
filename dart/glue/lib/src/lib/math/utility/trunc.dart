import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Truncate function (removes decimal part)
/// Mirrors Haskell Glue.Lib.Math.Utility.Trunc.trunc exactly
final Ir trunc = IrNativeFunc(truncImpl);

/// Truncate function implementation (removes decimal part)
/// Mirrors Haskell Glue.Lib.Math.Utility.Trunc.truncImpl exactly
Eval<Ir> truncImpl(Ir arg) {
  return switch (arg) {
    IrInteger(value: final n) => Eval.pure(IrInteger(n)),
    IrFloat(value: final n) => Eval.pure(IrInteger(n.truncate())),
    _ => throwError(wrongArgumentType(['number'])),
  };
}
