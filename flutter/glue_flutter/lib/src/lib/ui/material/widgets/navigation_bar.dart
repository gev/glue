import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// NavigationBar widget function
/// Creates Flutter NavigationBar from Glue (navigation-bar props) expressions
final Ir navigationBar = IrNativeFunc(navigationBarImpl);

/// NavigationBar implementation - takes properties object
Eval<Ir> navigationBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createNavigationBar(
    WidgetProperties(properties.unlock),
  ),
  _ => _createNavigationBar(WidgetProperties.empty()),
};

/// Create NavigationBar widget from properties
Eval<Ir> _createNavigationBar(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final navigationBarWidget = NavigationBar(
      key: properties.key,
      animationDuration: properties.getValue<Duration>('animation-duration'),
      selectedIndex: properties.getInt('selected-index') ?? 0,
      destinations: properties.getWidgets('destinations'),
      onDestinationSelected: properties
          .getCallback<int>('on-destination-selected')
          ?.call(runtime),
      backgroundColor: properties.getColor('background-color'),
      elevation: properties.getDouble('elevation'),
      shadowColor: properties.getColor('shadow-color'),
      surfaceTintColor: properties.getColor('surface-tint-color'),
      indicatorColor: properties.getColor('indicator-color'),
      indicatorShape: properties.getValue<ShapeBorder>('indicator-shape'),
      height: properties.getDouble('height'),
      labelBehavior: properties.getValue<NavigationDestinationLabelBehavior>(
        'label-behavior',
      ),
      overlayColor: properties.getValue<WidgetStateProperty<Color?>>(
        'overlay-color',
      ),
      labelTextStyle: properties.getValue<WidgetStateProperty<TextStyle>>(
        'label-text-style',
      ),
      labelPadding: properties.getValue<EdgeInsetsGeometry>('label-padding'),
      maintainBottomViewPadding:
          properties.getBool('maintain-bottom-view-padding') ?? false,
    );
    return IrNativeValue(Value(navigationBarWidget));
  });
}
