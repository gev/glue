import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/either.dart';
import 'package:glue/error.dart';
import 'package:glue_demo/services/state/state_notifier.dart';
import 'package:glue_demo/services/state/listenable_widget.dart';

/// Creates a ListenableWidget that rebuilds when dependencies change
/// Takes a StateNotifier and a lambda function that receives the current value
final listenFunction = IrNativeFunc((Ir notifierIr) {
  return Eval.pure(
    IrNativeFunc((Ir lambdaIr) {
      return Eval((runtime) {
        // Evaluate the notifier argument to get the actual StateNotifier object
        final notifier = switch (notifierIr) {
          IrNativeValue(value: final hv) => extractValue<StateNotifier>(hv),
          _ => null,
        };

        if (notifier == null) {
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

        // Create reactive widget that calls the lambda with current value
        final reactiveContainer = ListenableWidget(
          notifier: notifier,
          lambda: lambdaIr,
          runtime: runtime,
        );

        return Right((IrNativeValue(Value(reactiveContainer)), runtime));
      });
    }),
  );
});
