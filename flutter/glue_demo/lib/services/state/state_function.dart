import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'state_notifier.dart';

/// Creates a reactive state with HostValue getters
/// Returns IrNativeValue(HostValue(StateNotifier))
final stateFunction = IrNativeFunc((Ir initialValue) {
  final notifier = StateNotifier(initialValue);
  return Eval.pure(IrNativeValue(hostValue(notifier)));
});
