import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Placeholder widget function
/// Creates Flutter Placeholder from Glue (placeholder props) expressions
final Ir placeholder = IrNativeFunc(placeholderImpl);

/// Placeholder implementation - takes properties object
Eval<Ir> placeholderImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createPlaceholder(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Placeholder widget from properties
Eval<Ir> _createPlaceholder(WidgetProperties properties) {
  final placeholderWidget = Placeholder(
    key: properties.key,
    fallbackWidth: properties.getDouble('fallback-width') ?? 400.0,
    fallbackHeight: properties.getDouble('fallback-height') ?? 400.0,
    color: properties.getColor('color') ?? const Color(0xFF455A64),
    strokeWidth: properties.getDouble('stroke-width') ?? 2.0,
  );
  return Eval.pure(IrNativeValue(Value(placeholderWidget)));
}
