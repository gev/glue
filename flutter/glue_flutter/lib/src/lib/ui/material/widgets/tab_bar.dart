import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// TabBar widget function
/// Creates Flutter TabBar from Glue (tab-bar props) expressions
final Ir tabBar = IrNativeFunc(tabBarImpl);

/// TabBar implementation - takes properties object
Eval<Ir> tabBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTabBar(
    MaterialProperties(properties.unlock),
  ),
  _ => _createTabBar(MaterialProperties.empty()),
};

/// Create TabBar widget from properties
Eval<Ir> _createTabBar(MaterialProperties properties) {
  final tabBarWidget = TabBar(
    tabs: properties.tabBarTabs ?? [],
    controller: properties.tabBarController,
    isScrollable: properties.tabBarIsScrollable,
    padding: properties.tabBarPadding,
    indicatorColor: properties.tabBarIndicatorColor,
    automaticIndicatorColorAdjustment:
        properties.tabBarAutomaticIndicatorColorAdjustment,
    indicatorWeight: properties.tabBarIndicatorWeight,
    indicatorPadding: properties.tabBarIndicatorPadding,
    indicator: properties.tabBarIndicator,
    indicatorSize: properties.tabBarIndicatorSize,
    dividerColor: properties.tabBarDividerColor,
    dividerHeight: properties.tabBarDividerHeight,
    labelColor: properties.tabBarLabelColor,
    labelStyle: properties.tabBarLabelStyle,
    labelPadding: properties.tabBarLabelPadding,
    unselectedLabelColor: properties.tabBarUnselectedLabelColor,
    unselectedLabelStyle: properties.tabBarUnselectedLabelStyle,
    dragStartBehavior: properties.tabBarDragStartBehavior,
    overlayColor: properties.tabBarOverlayColor,
    mouseCursor: properties.tabBarMouseCursor,
    enableFeedback: properties.tabBarEnableFeedback,
    onTap: properties.tabBarOnTap,
    physics: properties.tabBarPhysics,
  );
  return Eval.pure(IrNativeValue(Value(tabBarWidget)));
}
