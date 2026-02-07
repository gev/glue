import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/lib/store/store.dart';

/// Put operation for store
final Ir put = IrNativeFunc(putImpl);

/// Put operation implementation - first argument (key)
Eval<Ir> putImpl(Ir key) {
  return Eval.pure(IrNativeFunc(putKey(key)));
}

/// Helper function for second argument (value)
Eval<Ir> Function(Ir) putKey(Ir key) {
  return (Ir value) => Eval.pure(IrNativeFunc(putValue(key, value)));
}

/// Helper function for third argument (store)
Eval<Ir> Function(Ir) putValue(Ir pathOrKey, Ir value) {
  return (Ir store) => switch (store) {
    IrNativeValue(value: Value(value: Store s)) => switch (pathOrKey) {
      IrList(elements: final path) =>
        s.putByPath(path.unlock, value)
            ? Eval.pure(IrVoid())
            : throwError(runtimeException('path-already-exists', pathOrKey)),
      _ =>
        s.put(pathOrKey, value)
            ? Eval.pure(IrVoid())
            : throwError(runtimeException('key-already-exists', pathOrKey)),
    },
    _ => throwError(wrongArgumentType(['path-or-key', 'value', 'store'])),
  };
}
