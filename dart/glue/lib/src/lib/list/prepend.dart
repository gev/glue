import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Prep function - preptructs a new list by prepending an element to a list
/// Mirrors Haskell Glue.Lib.List.Prepend.prepend exactly
Ir prepend = IrNativeFunc(prependImpl);

/// Prep function implementation
/// Mirrors Haskell Glue.Lib.List.Prepend.prependImpl exactly
Eval<Ir> prependImpl(Ir item) {
  return Eval.pure(IrNativeFunc(prependWith(item)));
}

/// Helper function for tail argument
/// Mirrors Haskell Glue.Lib.List.Prepend.prependWith exactly
Eval<Ir> Function(Ir) prependWith(Ir item) {
  return (Ir list) => switch (list) {
    IrList(elements: final elements) => Eval.pure(IrList([item, ...elements])),
    _ => throwError(wrongArgumentType(['any', 'list'])),
  };
}
