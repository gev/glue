import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// NavigationRail widget function
/// Creates Flutter NavigationRail from Glue (navigation-rail props) expressions
final Ir navigationRail = IrNativeFunc(navigationRailImpl);

/// NavigationRail implementation - takes properties object
Eval<Ir> navigationRailImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createNavigationRail(
    WidgetProperties(properties.unlock),
  ),
  _ => _createNavigationRail(WidgetProperties.empty()),
};

/// Create NavigationRail widget from properties
Eval<Ir> _createNavigationRail(WidgetProperties properties) {
  final navigationRailWidget = NavigationRail(
    backgroundColor: properties.navigationRailBackgroundColor,
    extended: properties.navigationRailExtended,
    leading: properties.navigationRailLeading,
    trailing: properties.navigationRailTrailing,
    destinations: properties.navigationRailDestinations ?? [],
    selectedIndex: properties.navigationRailSelectedIndex,
    onDestinationSelected: properties.navigationRailOnDestinationSelected,
    elevation: properties.navigationRailElevation,
    groupAlignment: properties.navigationRailGroupAlignment,
    labelType: properties.navigationRailLabelType,
    unselectedLabelTextStyle: properties.navigationRailUnselectedLabelTextStyle,
    selectedLabelTextStyle: properties.navigationRailSelectedLabelTextStyle,
    unselectedIconTheme: properties.navigationRailUnselectedIconTheme,
    selectedIconTheme: properties.navigationRailSelectedIconTheme,
    minWidth: properties.navigationRailMinWidth,
    minExtendedWidth: properties.navigationRailMinExtendedWidth,
    useIndicator: properties.navigationRailUseIndicator,
    indicatorColor: properties.navigationRailIndicatorColor,
    indicatorShape: properties.navigationRailIndicatorShape,
  );
  return Eval.pure(IrNativeValue(Value(navigationRailWidget)));
}
