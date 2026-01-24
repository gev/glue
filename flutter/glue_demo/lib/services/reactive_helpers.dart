import 'package:flutter/foundation.dart';
import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Helper function to extract ChangeNotifier from IrNativeValue
ChangeNotifier? extractChangeNotifier(Ir ir) {
  if (ir is IrNativeValue) {
    final hostValue = ir.value;
    final actualValue = hostValue.value;
    return actualValue is ChangeNotifier ? actualValue as ChangeNotifier : null;
  }
  return null;
}

/// Helper function to extract a single widget from Ir
Widget extractWidget(Ir ir) {
  if (ir is IrNativeValue) {
    final hostValue = ir.value;
    final actualValue = hostValue.value;
    if (actualValue is Widget) {
      return actualValue;
    }
  }
  return const Text('Invalid widget');
}

/// Helper function to extract list of widgets from Ir
/// This needs to evaluate each widget expression first
List<Widget> extractWidgetList(Ir ir) {
  if (ir is! IrList) return [];

  // For now, return empty list - we need to evaluate widget expressions
  // The child widgets need to be evaluated in the context where they can access variables
  return [const Text('Widgets not implemented yet')];
}
