import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Drawer widget function
/// Creates Flutter Drawer from Glue (drawer props) expressions
final Ir drawer = IrNativeFunc(drawerImpl);

/// Drawer implementation - takes properties object
Eval<Ir> drawerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createDrawer(
    WidgetProperties(properties.unlock),
  ),
  _ => _createDrawer(WidgetProperties.empty()),
};

/// Create Drawer widget from properties
Eval<Ir> _createDrawer(WidgetProperties properties) {
  final drawerWidget = Drawer(
    key: properties.key,
    backgroundColor: properties.getColor('background-color'),
    elevation: properties.getDouble('elevation'),
    shadowColor: properties.getColor('shadow-color'),
    surfaceTintColor: properties.getColor('surface-tint-color'),
    width: properties.getDouble('width'),
    shape: properties.getValue<ShapeBorder>('shape'),
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
    semanticLabel: properties.getString('semantic-label'),
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(drawerWidget)));
}
