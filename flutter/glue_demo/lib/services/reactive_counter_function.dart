import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/either.dart';
import 'reactive_counter.dart';

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
            (runtime) => Right((IrString(counter.value.toString()), runtime)),
          ),
          'increment': Eval(
            (runtime) => Right((
              IrNativeFunc((Ir amount) {
                final amt = switch (amount) {
                  IrInteger(value: final value) => value,
                  _ => 1,
                };
                counter.increment(amt);
                return Eval.pure(IrVoid());
              }),
              runtime,
            )),
          ),
          'decrement': Eval(
            (runtime) => Right((
              IrNativeFunc((Ir amount) {
                final amt = switch (amount) {
                  IrInteger(value: final value) => value,
                  _ => 1,
                };
                counter.decrement(amt);
                return Eval.pure(IrVoid());
              }),
              runtime,
            )),
          ),
        },
        setters: {
          'value': (Ir newValue) => Eval((runtime) {
            counter.value = (newValue as IrInteger).value;
            return Right((IrVoid(), runtime));
          }),
        },
      ),
    ),
  );
});
