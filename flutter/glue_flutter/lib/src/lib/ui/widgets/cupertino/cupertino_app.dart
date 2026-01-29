import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// CupertinoApp widget function
/// Creates Flutter CupertinoApp from Glue expressions
/// Expects keyword arguments: :home, :theme, :routes, :title, etc.
final Ir cupertinoApp = IrNativeFunc(cupertinoAppImpl);

/// CupertinoApp implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoAppImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoApp(
    Properties(properties.unlock),
  ),
  _ => _createCupertinoApp(Properties.empty()),
};

/// Create CupertinoApp widget from properties object
Eval<Ir> _createCupertinoApp(Properties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoApp(
      home: properties.child,
      title: 'Glue App', // Fixed string for now
    );
    return IrNativeValue(Value(widget));
  });
}
