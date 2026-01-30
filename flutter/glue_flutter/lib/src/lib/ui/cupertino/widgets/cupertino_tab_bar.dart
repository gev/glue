import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoTabBar widget function
/// Creates Flutter CupertinoTabBar from Glue expressions
/// Expects keyword arguments: :items, :on-press, :current-index, :active-color, :inactive-color, :icon-size, :border
final Ir cupertinoTabBar = IrNativeFunc(cupertinoTabBarImpl);

/// CupertinoTabBar implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoTabBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoTabBar(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoTabBar(WidgetProperties.empty()),
};

/// Create CupertinoTabBar widget from properties object
Eval<Ir> _createCupertinoTabBar(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoTabBar(
      key: properties.key,
      items: properties.getValue('items'),
      onTap: properties.getValue('on-tap'),
      currentIndex: properties.getInt('current-index') ?? 0,
      activeColor: properties.getColor('active-color'),
      inactiveColor:
          properties.getColor('inactive-color') ?? CupertinoColors.inactiveGray,
      iconSize: properties.getDouble('icon-size') ?? 30.0,
      border: properties.getValue('border'),
    );
    return IrNativeValue(Value(widget));
  });
}
