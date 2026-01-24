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

/// Helper function to extract list of widgets from Ir
/// This needs to evaluate each widget expression first
List<Widget> extractWidgetList(Ir ir) {
  if (ir is! IrList) return [];

  // For now, return empty list - we need to evaluate widget expressions
  // The child widgets need to be evaluated in the context where they can access variables
  return [const Text('Widgets not implemented yet')];
}
