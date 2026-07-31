import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// NavigationDestination widget function
/// Creates Flutter NavigationDestination from Glue (navigation-destination props) expressions
final Ir navigationDestination = IrNativeFunc(navigationDestinationImpl);

/// NavigationDestination implementation - takes properties object
Eval<Ir> navigationDestinationImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createNavigationDestination(
    WidgetProperties(properties.unlock),
  ),
  _ => _createNavigationDestination(WidgetProperties.empty()),
};

/// Create NavigationDestination widget from properties
Eval<Ir> _createNavigationDestination(WidgetProperties properties) {
  final icon = properties.getWidget('icon');
  if (icon == null) {
    return throwError(wrongArgumentType(['`icon` property required']));
  }
  final label = properties.getString('label');
  if (label == null) {
    return throwError(wrongArgumentType(['`label` property required']));
  }
  final navigationDestinationWidget = NavigationDestination(
    key: properties.key,
    icon: icon,
    selectedIcon: properties.getWidget('selected-icon'),
    label: label,
    tooltip: properties.getString('tooltip'),
    enabled: properties.getBool('enabled') ?? true,
  );
  return Eval.pure(IrNativeValue(Value(navigationDestinationWidget)));
}
