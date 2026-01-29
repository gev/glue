import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// BottomAppBar widget function
/// Creates Flutter BottomAppBar from Glue (bottom-app-bar props) expressions
final Ir bottomAppBar = IrNativeFunc(bottomAppBarImpl);

/// BottomAppBar implementation - takes properties object
Eval<Ir> bottomAppBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBottomAppBar(
    WidgetProperties(properties.unlock),
  ),
  _ => _createBottomAppBar(WidgetProperties.empty()),
};

/// Create BottomAppBar widget from properties
Eval<Ir> _createBottomAppBar(WidgetProperties properties) {
  final bottomAppBarWidget = BottomAppBar(
    key: properties.key,
    color: properties.getColor('color'),
    elevation: properties.getDouble('elevation'),
    shape: properties.getValue('shape'),
    clipBehavior: properties.getValue('clip-behavior'),
    notchMargin: properties.getDouble('notch-margin') ?? 4.0,
    height: properties.getDouble('height'),
    padding: properties.getValue('padding'),
    surfaceTintColor: properties.getColor('surface-tint-color'),
    shadowColor: properties.getColor('shadow-color'),
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(bottomAppBarWidget)));
}
