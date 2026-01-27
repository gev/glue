import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/either.dart';
import 'package:glue/error.dart';
import 'package:glue_demo/services/state/state_notifier.dart';

/// Reads the value of a reactive state
/// Takes: state (IrNativeValue), returns the current value
/// Returns: Ir (the current state value)
final readFunction = IrNativeFunc((Ir stateIr) {
  return Eval((runtime) {
    final state = switch (stateIr) {
      IrNativeValue(value: final hv) => extractValue<StateNotifier>(hv),
      _ => null,
    };
    if (state == null) {
      return Left(
        EvalError(
          [],
          RuntimeException(
            'invalid-argument',
            IrString('Expected StateNotifier'),
          ),
        ),
      );
    }
    return Right((state.value, runtime));
  });
});
