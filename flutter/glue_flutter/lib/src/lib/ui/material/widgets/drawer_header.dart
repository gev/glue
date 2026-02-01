import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// DrawerHeader widget function
/// Creates Flutter DrawerHeader from Glue (drawer-header props) expressions
final Ir drawerHeader = IrNativeFunc(drawerHeaderImpl);

/// DrawerHeader implementation - takes properties object
Eval<Ir> drawerHeaderImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createDrawerHeader(
    WidgetProperties(properties.unlock),
  ),
  _ => _createDrawerHeader(WidgetProperties.empty()),
};

/// Create DrawerHeader widget from properties
Eval<Ir> _createDrawerHeader(WidgetProperties properties) {
  final drawerHeaderWidget = DrawerHeader(
    key: properties.key,
    decoration: properties.getValue<>('decoration'),
    margin: properties.getValue<>('margin') ?? const EdgeInsets.only(bottom: 8.0),
    padding:
        properties.getValue<>('padding') ??
        const EdgeInsets.fromLTRB(16.0, 16.0, 16.0, 8.0),
    duration:
        properties.getValue<>('duration') ?? const Duration(milliseconds: 250),
    curve: properties.getValue<>('curve') ?? Curves.fastOutSlowIn,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(drawerHeaderWidget)));
}
