import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// TabBarView widget function
/// Creates Flutter TabBarView from Glue (tab-bar-view props) expressions
final Ir tabBarView = IrNativeFunc(tabBarViewImpl);

/// TabBarView implementation - takes properties object
Eval<Ir> tabBarViewImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTabBarView(
    WidgetProperties(properties.unlock),
  ),
  _ => _createTabBarView(WidgetProperties.empty()),
};

/// Create TabBarView widget from properties
Eval<Ir> _createTabBarView(WidgetProperties properties) {
  final tabBarViewWidget = TabBarView(
    children: properties.tabBarViewChildren ?? [],
    controller: properties.tabBarViewController,
    physics: properties.tabBarViewPhysics,
    dragStartBehavior: properties.tabBarViewDragStartBehavior,
    viewportFraction: properties.tabBarViewViewportFraction,
  );
  return Eval.pure(IrNativeValue(Value(tabBarViewWidget)));
}
