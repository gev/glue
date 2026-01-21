import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Cons function - constructs a new list by prepending an element to a list
/// Mirrors Haskell Glue.Lib.List.Cons.cons exactly
Ir cons = IrNativeFunc(consImpl);

/// Cons function implementation
/// Mirrors Haskell Glue.Lib.List.Cons.consImpl exactly
Eval<Ir> consImpl(Ir head) {
  return Eval.pure(IrNativeFunc(consWith(head)));
}

/// Helper function for tail argument
/// Mirrors Haskell Glue.Lib.List.Cons.consWith exactly
Eval<Ir> Function(Ir) consWith(Ir head) {
  return (Ir tail) => switch (tail) {
    IrList(elements: final elements) => Eval.pure(IrList([head, ...elements])),
    _ => throwError(wrongArgumentType(['list'])),
  };
}
