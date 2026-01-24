import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Helper function to extract ChangeNotifier from IrNativeValue
ChangeNotifier? extractChangeNotifier(Ir ir) {
  return switch (ir) {
    IrNativeValue(value: HostValue(value: ChangeNotifier notifier)) => notifier,
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
