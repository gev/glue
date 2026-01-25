import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/either.dart';
import 'package:glue/error.dart';
import 'counter_notifier.dart';

/// Takes: counter (IrNativeValue), returns function that takes amount
/// Returns: IrVoid
final dec = IrNativeFunc((Ir counterIr) {
  return Eval.pure(
    IrNativeFunc((Ir amountIr) {
      final amount = switch (amountIr) {
        IrInteger(value: final value) => value,
        _ => 1,
      };
      return Eval((runtime) {
        final counter = switch (counterIr) {
          IrNativeValue(value: final hv) => extractHostValue<CounterNotifier>(
            hv,
          ),
          _ => null,
        };
        if (counter == null) {
          return Left(
            EvalError(
              [],
              RuntimeException(
                'invalid-argument',
                IrString('Expected ReactiveCounter'),
              ),
            ),
          );
        }

        counter.decrement(amount);
        return Right((IrVoid(), runtime));
      });
    }),
  );
});
