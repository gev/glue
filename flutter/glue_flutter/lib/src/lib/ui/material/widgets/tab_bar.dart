import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// TabBar widget function
/// Creates Flutter TabBar from Glue (tab-bar props) expressions
final Ir tabBar = IrNativeFunc(tabBarImpl);

/// TabBar implementation - takes properties object
Eval<Ir> tabBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTabBar(
    WidgetProperties(properties.unlock),
  ),
  _ => _createTabBar(WidgetProperties.empty()),
};

/// Create TabBar widget from properties
Eval<Ir> _createTabBar(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final tabBarWidget = TabBar(
      key: properties.key,
      tabs: properties.getWidgets('tabs') ?? [],
      controller: properties.getValue('controller'),
      isScrollable: properties.getBool('is-scrollable') ?? false,
      padding: properties.getValue('padding'),
      indicatorColor: properties.getColor('indicator-color'),
      automaticIndicatorColorAdjustment:
          properties.getBool('automatic-indicator-color-adjustment') ?? true,
      indicatorWeight: properties.getDouble('indicator-weight') ?? 2.0,
      indicatorPadding:
          properties.getValue('indicator-padding') ?? EdgeInsets.zero,
      indicator: properties.getValue('indicator'),
      indicatorSize: properties.getValue('indicator-size'),
      dividerColor: properties.getColor('divider-color'),
      dividerHeight: properties.getDouble('divider-height'),
      labelColor: properties.getColor('label-color'),
      labelStyle: properties.getValue('label-style'),
      labelPadding: properties.getValue('label-padding'),
      unselectedLabelColor: properties.getColor('unselected-label-color'),
      unselectedLabelStyle: properties.getValue('unselected-label-style'),
      dragStartBehavior:
          properties.getValue('drag-start-behavior') ?? DragStartBehavior.start,
      overlayColor: properties.getValue('overlay-color'),
      mouseCursor: properties.getValue('mouse-cursor'),
      enableFeedback: properties.getBool('enable-feedback'),
      onTap: properties.getValue('on-tap'),
      physics: properties.getValue('physics'),
    );
    return IrNativeValue(Value(tabBarWidget));
  });
}
