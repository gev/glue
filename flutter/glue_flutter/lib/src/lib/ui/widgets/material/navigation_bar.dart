import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// NavigationBar widget function
/// Creates Flutter NavigationBar from Glue (navigation-bar props) expressions
final Ir navigationBar = IrNativeFunc(navigationBarImpl);

/// NavigationBar implementation - takes properties object
Eval<Ir> navigationBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createNavigationBar(
    MaterialProperties(properties.unlock),
  ),
  _ => _createNavigationBar(MaterialProperties.empty()),
};

/// Create NavigationBar widget from properties
Eval<Ir> _createNavigationBar(MaterialProperties properties) {
  final navigationBarWidget = NavigationBar(
    animationDuration: properties.navigationBarAnimationDuration,
    selectedIndex: properties.navigationBarSelectedIndex,
    destinations: properties.navigationBarDestinations ?? [],
    onDestinationSelected: properties.navigationBarOnDestinationSelected,
    backgroundColor: properties.navigationBarBackgroundColor,
    elevation: properties.navigationBarElevation,
    shadowColor: properties.navigationBarShadowColor,
    surfaceTintColor: properties.navigationBarSurfaceTintColor,
    indicatorColor: properties.navigationBarIndicatorColor,
    indicatorShape: properties.navigationBarIndicatorShape,
    height: properties.navigationBarHeight,
    labelBehavior: properties.navigationBarLabelBehavior,
    overlayColor: properties.navigationBarOverlayColor,
    labelTextStyle: properties.navigationBarLabelTextStyle,
    labelPadding: properties.navigationBarLabelPadding,
    maintainBottomViewPadding:
        properties.navigationBarMaintainBottomViewPadding,
  );
  return Eval.pure(IrNativeValue(Value(navigationBarWidget)));
}
