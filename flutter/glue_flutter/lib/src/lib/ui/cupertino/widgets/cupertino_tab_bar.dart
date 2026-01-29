import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/cupertino_properties.dart';

/// CupertinoTabBar widget function
/// Creates Flutter CupertinoTabBar from Glue expressions
/// Expects keyword arguments: :items, :on-press, :current-index, :active-color, :inactive-color, :icon-size, :border
final Ir cupertinoTabBar = IrNativeFunc(cupertinoTabBarImpl);

/// CupertinoTabBar implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoTabBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoTabBar(
    CupertinoProperties(properties.unlock),
  ),
  _ => _createCupertinoTabBar(CupertinoProperties.empty()),
};

/// Create CupertinoTabBar widget from properties object
Eval<Ir> _createCupertinoTabBar(CupertinoProperties properties) {
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
