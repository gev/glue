import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// TabBarView widget function
/// Creates Flutter TabBarView from Glue (tab-bar-view props) expressions
final Ir tabBarView = IrNativeFunc(tabBarViewImpl);

/// TabBarView implementation - takes properties object
Eval<Ir> tabBarViewImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTabBarView(
    MaterialProperties(properties.unlock),
  ),
  _ => _createTabBarView(MaterialProperties.empty()),
};

/// Create TabBarView widget from properties
Eval<Ir> _createTabBarView(MaterialProperties properties) {
  final tabBarViewWidget = TabBarView(
    children: properties.tabBarViewChildren ?? [],
    controller: properties.tabBarViewController,
    physics: properties.tabBarViewPhysics,
    dragStartBehavior: properties.tabBarViewDragStartBehavior,
    viewportFraction: properties.tabBarViewViewportFraction,
  );
  return Eval.pure(IrNativeValue(Value(tabBarViewWidget)));
}
