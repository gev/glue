import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';
import 'package:glue_demo/services/state/state_notifier.dart';

/// Helper function to extract ChangeNotifier from IrNativeValue
ChangeNotifier? extractStateNotifier(Ir ir) {
  return switch (ir) {
    IrNativeValue(value: HostValue(value: StateNotifier notifier)) => notifier,
    _ => null,
  };
}

/// Helper function to extract a single widget from Ir
Widget extractWidget(Ir ir) {
  return switch (ir) {
    IrNativeValue(value: HostValue(value: Widget widget)) => widget,
    _ => const Text('Invalid widget'),
  };
}
