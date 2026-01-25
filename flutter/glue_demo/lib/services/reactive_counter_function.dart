import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/either.dart';
import 'package:glue/error.dart';
import 'reactive_counter.dart';

// Helper function for throwing runtime errors
Eval<Ir> throwError(RuntimeException exception) {
  return Eval((runtime) => Left(EvalError([], exception)));
}

/// Creates a reactive counter with HostValue getters and setters
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
