import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';

/// Logical not function
/// Mirrors Haskell Glue.Lib.Bool.Not.not_ exactly
final Ir not = IrNativeFunc(notImpl);

/// Logical not implementation
/// Mirrors Haskell Glue.Lib.Bool.Not.notImpl exactly
Eval<Ir> notImpl(Ir arg) => Eval.pure(IrBool(isFalsy(arg)));
