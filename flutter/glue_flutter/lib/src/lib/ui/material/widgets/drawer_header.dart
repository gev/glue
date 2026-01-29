import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// DrawerHeader widget function
/// Creates Flutter DrawerHeader from Glue (drawer-header props) expressions
final Ir drawerHeader = IrNativeFunc(drawerHeaderImpl);

/// DrawerHeader implementation - takes properties object
Eval<Ir> drawerHeaderImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createDrawerHeader(
    MaterialProperties(properties.unlock),
  ),
  _ => _createDrawerHeader(MaterialProperties.empty()),
};

/// Create DrawerHeader widget from properties
Eval<Ir> _createDrawerHeader(MaterialProperties properties) {
  final drawerHeaderWidget = DrawerHeader(
    decoration: properties.drawerHeaderDecoration,
    margin: properties.drawerHeaderMargin ?? const EdgeInsets.only(bottom: 8.0),
    padding:
        properties.drawerHeaderPadding ??
        const EdgeInsets.fromLTRB(16.0, 16.0, 16.0, 8.0),
    duration:
        properties.drawerHeaderDuration ?? const Duration(milliseconds: 250),
    curve: properties.drawerHeaderCurve ?? Curves.fastOutSlowIn,
    child: properties.drawerHeaderChild,
  );
  return Eval.pure(IrNativeValue(Value(drawerHeaderWidget)));
}
