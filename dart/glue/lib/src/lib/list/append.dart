import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Prep function - preptructs a new list by appending an element to a list
/// Mirrors Haskell Glue.Lib.List.Append.append exactly
Ir append = IrNativeFunc(appendImpl);

/// Prep function implementation
/// Mirrors Haskell Glue.Lib.List.Append.appendImpl exactly
Eval<Ir> appendImpl(Ir list) {
  return Eval.pure(IrNativeFunc(appendWith(list)));
}

/// Helper function for tail argument
/// Mirrors Haskell Glue.Lib.List.Append.appendWith exactly
Eval<Ir> Function(Ir) appendWith(Ir list) {
  return (Ir item) => switch (list) {
    IrList(elements: final elements) => Eval.pure(IrList([...elements, item])),
    _ => throwError(wrongArgumentType(['list', 'any'])),
  };
}
