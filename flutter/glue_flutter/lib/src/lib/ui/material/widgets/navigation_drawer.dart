import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// NavigationDrawer widget function
/// Creates Flutter NavigationDrawer from Glue (navigation-drawer props) expressions
final Ir navigationDrawer = IrNativeFunc(navigationDrawerImpl);

/// NavigationDrawer implementation - takes properties object
Eval<Ir> navigationDrawerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createNavigationDrawer(
    WidgetProperties(properties.unlock),
  ),
  _ => _createNavigationDrawer(WidgetProperties.empty()),
};

/// Create NavigationDrawer widget from properties
Eval<Ir> _createNavigationDrawer(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final navigationDrawerWidget = NavigationDrawer(
      key: properties.key,
      backgroundColor: properties.getColor('background-color'),
      elevation: properties.getDouble('elevation'),
      shadowColor: properties.getColor('shadow-color'),
      surfaceTintColor: properties.getColor('surface-tint-color'),
      indicatorColor: properties.getColor('indicator-color'),
      indicatorShape: properties.getValue<ShapeBorder>('indicator-shape'),
      selectedIndex: properties.getInt('selected-index'),
      onDestinationSelected: properties
          .getCallback<int>('on-destination-selected')
          ?.call(runtime),
      tilePadding:
          properties.getValue<EdgeInsetsGeometry>('tile-padding') ??
          EdgeInsets.symmetric(horizontal: 12.0),
      header: properties.getWidget('header'),
      children: properties.children,
      footer: properties.getWidget('footer'),
    );
    return IrNativeValue(Value(navigationDrawerWidget));
  });
}
