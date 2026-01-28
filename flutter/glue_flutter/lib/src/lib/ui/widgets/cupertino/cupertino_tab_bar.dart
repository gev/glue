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
      currentIndex: properties.cupertinoTabBarCurrentIndex ?? 0,
      activeColor:
          properties.cupertinoTabBarActiveColor ?? CupertinoColors.activeBlue,
      inactiveColor:
          properties.cupertinoTabBarInactiveColor ??
          CupertinoColors.inactiveGray,
      iconSize: properties.cupertinoTabBarIconSize ?? 30.0,
      border:
          properties.cupertinoTabBarBorder ??
          const Border(top: BorderSide(color: Color(0x4D000000), width: 0.0)),
    );
    return IrNativeValue(Value(widget));
  });
}
