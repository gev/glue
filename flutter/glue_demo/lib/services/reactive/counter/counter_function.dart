import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/either.dart';
import 'counter_notifier.dart';

/// Creates a reactive counter with HostValue getters
/// Returns IrNativeValue(HostValue(ReactiveCounter))
final counterFunction = IrNativeFunc((Ir initialValue) {
  final initial = switch (initialValue) {
    IrInteger(value: final value) => value,
    _ => 0,
  };
  final counter = CounterNotifier(initial);
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
