import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/lib/store/store.dart';

/// Get operation for store
final Ir get = IrNativeFunc(getImpl);

/// Get operation implementation - first argument (key)
Eval<Ir> getImpl(Ir key) {
  return Eval.pure(IrNativeFunc(getKey(key)));
}

/// Helper function for second argument (store)
Eval<Ir> Function(Ir) getKey(Ir key) {
  return (Ir store) => switch (store) {
    IrNativeValue(value: Store s) => () {
      final value = s.get(key);
      return Eval.pure(value ?? IrVoid());
    }(),
    _ => throwError(wrongArgumentType(['key', 'store'])),
  };
}
