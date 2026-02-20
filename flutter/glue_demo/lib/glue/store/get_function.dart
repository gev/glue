import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_demo/glue/store/store.dart';

/// Get operation for store
final Ir get = IrNativeFunc(getImpl);

/// Get operation implementation - first argument (key)
Eval<Ir> getImpl(Ir key) {
  return Eval.pure(IrNativeFunc(getKey(key)));
}

/// Helper function for second argument (store)
Eval<Ir> Function(Ir) getKey(Ir pathOrKey) {
  return (Ir store) => switch (store) {
    IrNativeValue(value: Value(value: Store s)) => switch (pathOrKey) {
      IrList(elements: final path) => Eval.pure(
        s.getByPath(path.unlock) ?? IrVoid(),
      ),
      _ => Eval.pure(s.get(pathOrKey) ?? IrVoid()),
    },
    _ => throwError(wrongArgumentType(['path-or-key', 'store'])),
  };
}
