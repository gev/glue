import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

/// ButtonSegment function
/// Creates Flutter ButtonSegment from Glue (button-segment props) expressions
final Ir buttonSegment = IrNativeFunc(buttonSegmentImpl);

/// ButtonSegment implementation - takes properties object
Eval<Ir> buttonSegmentImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createButtonSegment(
    WidgetProperties(properties.unlock),
  ),
  _ => _createButtonSegment(WidgetProperties.empty()),
};

/// Create ButtonSegment from properties
Eval<Ir> _createButtonSegment(WidgetProperties properties) {
  // ButtonSegment не требует runtime для создания, но оборачиваем для консистентности стиля
  return getRuntime().map((runtime) {
    final segment = ButtonSegment<Ir>(
      value: properties.getValue<Ir>('value')!,
      label: properties.getValue<Widget>('label'),
      icon: properties.getValue<Widget>('icon'),
      tooltip: properties.getValue<String>('tooltip'),
      enabled: properties.getBool('enabled') ?? true,
    );
    return IrNativeValue(Value(segment));
  });
}
