import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// Drawer widget function
/// Creates Flutter Drawer from Glue (drawer props) expressions
final Ir drawer = IrNativeFunc(drawerImpl);

/// Drawer implementation - takes properties object
Eval<Ir> drawerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createDrawer(
    MaterialProperties(properties.unlock),
  ),
  _ => _createDrawer(MaterialProperties.empty()),
};

/// Create Drawer widget from properties
Eval<Ir> _createDrawer(MaterialProperties properties) {
  final drawerWidget = Drawer(
    key: properties.focusNode != null
        ? Key(properties.focusNode.toString())
        : null,
    backgroundColor: properties.drawerBackgroundColor,
    elevation: properties.drawerElevation,
    shadowColor: properties.drawerShadowColor,
    surfaceTintColor: properties.drawerSurfaceTintColor,
    width: properties.drawerWidth,
    shape: properties.drawerShape,
    clipBehavior: properties.drawerClipBehavior,
    semanticLabel: properties.drawerSemanticLabel.toString(),
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(drawerWidget)));
}
