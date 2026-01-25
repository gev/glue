import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/either.dart';
import 'package:glue/error.dart';
import 'reactive_counter.dart';

// Helper function for throwing runtime errors
Eval<Ir> throwError(RuntimeException exception) {
  return Eval((runtime) => Left(EvalError([], exception)));
}

/// Creates a reactive counter with HostValue getters
/// Returns IrNativeValue(HostValue(ReactiveCounter))
final reactiveCounter = IrNativeFunc((Ir initialValue) {
  final initial = switch (initialValue) {
    IrInteger(value: final value) => value,
    _ => 0,
  };
  final counter = ReactiveCounter(initial);

  return Eval.pure(
    IrNativeValue(
      HostValue(
        counter,
        getters: {
          'value': Eval(
            (runtime) => Right((IrInteger(counter.value), runtime)),
          ),
        },
      ),
    ),
  );
});

/// Increments a reactive counter by the specified amount (default 1)
/// Takes: counter (IrNativeValue), returns function that takes amount
/// Returns: IrVoid
final inc = IrNativeFunc((Ir counterIr) {
  return Eval.pure(
    IrNativeFunc((Ir amountIr) {
      final amount = switch (amountIr) {
        IrInteger(value: final value) => value,
        _ => 1,
      };

      return Eval((runtime) {
        final counter = switch (counterIr) {
          IrNativeValue(value: final hv) => extractHostValue<ReactiveCounter>(
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

        counter.increment(amount);
        return Right((IrVoid(), runtime));
      });
    }),
  );
});

/// Decrements a reactive counter by the specified amount (default 1)
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
          IrNativeValue(value: final hv) => extractHostValue<ReactiveCounter>(
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
