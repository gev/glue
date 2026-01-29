import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// NavigationDrawer widget function
/// Creates Flutter NavigationDrawer from Glue (navigation-drawer props) expressions
final Ir navigationDrawer = IrNativeFunc(navigationDrawerImpl);

/// NavigationDrawer implementation - takes properties object
Eval<Ir> navigationDrawerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createNavigationDrawer(
    Properties(properties.unlock),
  ),
  _ => _createNavigationDrawer(Properties.empty()),
};

/// Create NavigationDrawer widget from properties
Eval<Ir> _createNavigationDrawer(Properties properties) {
  final navigationDrawerWidget = NavigationDrawer(
    backgroundColor: properties.navigationDrawerBackgroundColor,
    elevation: properties.navigationDrawerElevation,
    shadowColor: properties.navigationDrawerShadowColor,
    surfaceTintColor: properties.navigationDrawerSurfaceTintColor,
    indicatorColor: properties.navigationDrawerIndicatorColor,
    indicatorShape: properties.navigationDrawerIndicatorShape,
    selectedIndex: properties.navigationDrawerSelectedIndex,
    onDestinationSelected: properties.navigationDrawerOnDestinationSelected,
    tilePadding:
        properties.navigationDrawerTilePadding ??
        const EdgeInsets.symmetric(horizontal: 12.0),
    children: properties.navigationDrawerChildren ?? [],
  );
  return Eval.pure(IrNativeValue(Value(navigationDrawerWidget)));
}
