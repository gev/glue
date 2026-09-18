import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Concat function - concatenates two lists, objects or strings
/// Mirrors Haskell Glue.Lib.List.Concat.concat exactly
Ir concat = IrNativeFunc(concatImpl);

/// Concat function implementation
/// Mirrors Haskell Glue.Lib.List.Concat.concatImpl exactly
Eval<Ir> concatImpl(Ir ir1) {
  return Eval.pure(IrNativeFunc(concatWith(ir1)));
}

/// Helper function for second argument
/// Mirrors Haskell Glue.Lib.Builtin.Concat.concatWith exactly
Eval<Ir> Function(Ir) concatWith(Ir ir1) {
  return (Ir ir2) => switch ((ir1, ir2)) {
    (IrObject(properties: final p1), IrObject(properties: final p2)) =>
      Eval.pure(IrObject(p1.addAll(p2, keepOrder: true).unlock)),
    (IrString(value: final v1), IrString(value: final v2)) => Eval.pure(
      IrString(v1 + v2),
    ),
    (IrList(elements: final e1), IrList(elements: final e2)) => Eval.pure(
      IrList([...e1, ...e2]),
    ),
    _ => throwError(wrongArgumentType(['Lists, objects or strings required'])),
  };
}
