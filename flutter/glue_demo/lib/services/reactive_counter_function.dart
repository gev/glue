import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/either.dart';
import 'reactive_counter.dart';

/// Creates a reactive counter with HostValue getters and setters
/// Returns IrNativeValue(HostValue(ReactiveCounter))
final reactiveCounter = IrNativeFunc((Ir initialValue) {
  final initial = initialValue is IrInteger ? initialValue.value : 0;
  final counter = ReactiveCounter(initial);

  return Eval.pure(
    IrNativeValue(
      HostValue(
        counter,
        getters: {
          'value': Eval(
            (runtime) => Right((IrInteger(counter.value), runtime)),
          ),
          'increment': Eval(
            (runtime) => Right((
              IrNativeFunc((Ir amount) {
                final amt = amount is IrInteger ? amount.value : 1;
                counter.increment(amt);
                return Eval.pure(IrVoid());
              }),
              runtime,
            )),
          ),
          'decrement': Eval(
            (runtime) => Right((
              IrNativeFunc((Ir amount) {
                final amt = amount is IrInteger ? amount.value : 1;
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
