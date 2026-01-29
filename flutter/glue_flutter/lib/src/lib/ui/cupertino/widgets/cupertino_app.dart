import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoApp widget function
/// Creates Flutter CupertinoApp from Glue expressions
/// Expects keyword arguments: :home, :theme, :routes, :title, etc.
final Ir cupertinoApp = IrNativeFunc(cupertinoAppImpl);

/// CupertinoApp implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoAppImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoApp(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoApp(WidgetProperties.empty()),
};

/// Create CupertinoApp widget from properties object
Eval<Ir> _createCupertinoApp(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoApp(
      home: properties.child,
      title: 'Glue App', // Fixed string for now
    );
    return IrNativeValue(Value(widget));
  });
}
