import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Member function - checks if an item is in a list
/// Mirrors Haskell Glue.Lib.List.Member.member exactly
Ir member = IrNativeFunc(memberImpl);

/// Member function implementation
/// Mirrors Haskell Glue.Lib.List.Member.memberImpl exactly
Eval<Ir> memberImpl(Ir itemIr) {
  return Eval.pure(IrNativeFunc(memberIn(itemIr)));
}

/// Helper function for list argument
/// Mirrors Haskell Glue.Lib.List.Member.memberIn exactly
Eval<Ir> Function(Ir) memberIn(Ir item) {
  return (Ir list) => switch (list) {
    IrList(:final elements) => Eval.pure(IrBool(elements.contains(item))),
    _ => throwError(wrongArgumentType(['list'])),
  };
}
