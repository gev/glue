import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoTabBar widget function
/// Creates Flutter CupertinoTabBar from Glue expressions
/// Expects keyword arguments: :items, :on-tap, :current-index, :active-color, :inactive-color, :icon-size, :border
final Ir cupertinoTabBar = IrNativeFunc(cupertinoTabBarImpl);

/// CupertinoTabBar implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoTabBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoTabBar(
    Properties(properties.unlock),
  ),
  _ => _createCupertinoTabBar(Properties.empty()),
};

/// Create CupertinoTabBar widget from properties object
Eval<Ir> _createCupertinoTabBar(Properties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoTabBar(
      items: properties.cupertinoTabBarItems,
      onTap: properties.cupertinoTabBarOnTap,
      currentIndex: properties.cupertinoTabBarCurrentIndex,
      activeColor: properties.cupertinoTabBarActiveColor,
      inactiveColor: properties.cupertinoTabBarInactiveColor,
      iconSize: properties.cupertinoTabBarIconSize,
      border: properties.cupertinoTabBarBorder,
    );
    return IrNativeValue(Value(widget));
  });
}
