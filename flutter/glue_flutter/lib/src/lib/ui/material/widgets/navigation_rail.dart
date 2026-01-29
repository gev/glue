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
  return getRuntime().map((runtime) {
    final navigationRailWidget = NavigationRail(
      key: properties.key,
      backgroundColor: properties.getColor('background-color'),
      extended: properties.getBool('extended') ?? false,
      leading: properties.getWidget('leading'),
      trailing: properties.getWidget('trailing'),
      destinations: properties.getValue('destinations') ?? [],
      selectedIndex: properties.getInt('selected-index'),
      onDestinationSelected: properties.getValue('on-destination-selected'),
      elevation: properties.getDouble('elevation'),
      groupAlignment: properties.getDouble('group-alignment'),
      labelType: properties.getValue('label-type'),
      unselectedLabelTextStyle: properties.getValue(
        'unselected-label-text-style',
      ),
      selectedLabelTextStyle: properties.getValue('selected-label-text-style'),
      unselectedIconTheme: properties.getValue('unselected-icon-theme'),
      selectedIconTheme: properties.getValue('selected-icon-theme'),
      minWidth: properties.getDouble('min-width'),
      minExtendedWidth: properties.getDouble('min-extended-width'),
      useIndicator: properties.getBool('use-indicator'),
      indicatorColor: properties.getColor('indicator-color'),
      indicatorShape: properties.getValue('indicator-shape'),
    );
    return IrNativeValue(Value(navigationRailWidget));
  });
}
