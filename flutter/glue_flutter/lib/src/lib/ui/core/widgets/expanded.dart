import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Expanded widget function
/// Creates Flutter Expanded from Glue (expanded props) expressions
final Ir expanded = IrNativeFunc(expandedImpl);

/// Expanded implementation - takes properties object
Eval<Ir> expandedImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createExpanded(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Expanded widget from properties
Eval<Ir> _createExpanded(WidgetProperties properties) {
  final child = properties.child;
  if (child == null) {
    return throwError(wrongArgumentType(['child property required']));
  }
  final expandedWidget = Expanded(
    key: properties.key,
    flex: properties.getInt('flex') ?? 1,
    child: child,
  );
  return Eval.pure(IrNativeValue(Value(expandedWidget)));
}
