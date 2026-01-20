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
Eval<Ir> Function(Ir) memberIn(Ir itemIr) {
  return (Ir listIr) {
    return sequenceAll([eval(itemIr), eval(listIr)]).flatMap((evaluated) {
      return switch (evaluated) {
        [final item, final list] =>
          list is IrList
              ? Eval.pure(IrBool(list.elements.contains(item)))
              : throwError(wrongArgumentType(['list'])),
        _ => throwError(wrongArgumentType(['list'])),
      };
    });
  };
}
