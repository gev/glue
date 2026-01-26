import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/either.dart';
import 'package:glue/error.dart';
import 'package:glue_demo/services/state/state_notifier.dart';

/// Modifies a reactive state atomically
/// Takes: state (IrNativeValue), returns function that takes transformation lambda
/// Returns: IrVoid
/// Usage: (modify state (lambda (current-value) new-value))
final modifyFunction = IrNativeFunc((Ir stateIr) {
  return Eval.pure(
    IrNativeFunc((Ir lambdaIr) {
      return Eval((runtime) async {
        final state = switch (stateIr) {
          IrNativeValue(value: final hv) => extractHostValue<StateNotifier>(hv),
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
        final result = await runEval(apply(lambdaIr, [state.value]), runtime);
        return result.match((error) => Left(error), (value) {
          state.value = value.$1;
          return Right((IrVoid(), runtime));
        });
      });
    }),
  );
});
