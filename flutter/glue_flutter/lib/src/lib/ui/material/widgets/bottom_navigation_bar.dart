import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// BottomNavigationBar widget function
/// Creates Flutter BottomNavigationBar from Glue (bottom-navigation-bar props) expressions
final Ir bottomNavigationBar = IrNativeFunc(bottomNavigationBarImpl);

/// BottomNavigationBar implementation - takes properties object
Eval<Ir> bottomNavigationBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBottomNavigationBar(
    WidgetProperties(properties.unlock),
  ),
  _ => _createBottomNavigationBar(WidgetProperties.empty()),
};

/// Create BottomNavigationBar widget from properties
Eval<Ir> _createBottomNavigationBar(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final bottomNavigationBarWidget = BottomNavigationBar(
      key: properties.key,
      items: properties.getValues<BottomNavigationBarItem>('items'),
      onTap: properties.getCallback<int>('on-tap')?.call(runtime),
      currentIndex: properties.getInt('current-index') ?? 0,
      elevation: properties.getDouble('elevation') ?? 8,
      type: properties.getValue<BottomNavigationBarType>('type'),
      fixedColor: properties.getColor('fixed-color'),
      backgroundColor: properties.getColor('background-color'),
      iconSize: properties.getDouble('icon-size') ?? 24,
      selectedItemColor: properties.getColor('selected-item-color'),
      unselectedItemColor: properties.getColor('unselected-item-color'),
      selectedIconTheme: properties.getValue<IconThemeData>(
        'selected-icon-theme',
      ),
      unselectedIconTheme: properties.getValue<IconThemeData>(
        'unselected-icon-theme',
      ),
      selectedLabelStyle: properties.getValue<TextStyle>(
        'selected-label-style',
      ),
      unselectedLabelStyle: properties.getValue<TextStyle>(
        'unselected-label-style',
      ),
      selectedFontSize: (properties.getDouble('selected-font-size') ?? 14),
      unselectedFontSize: (properties.getDouble('unselected-font-size') ?? 12),
      showSelectedLabels: properties.getBool('show-selected-labels'),
      showUnselectedLabels: properties.getBool('show-unselected-labels'),
      enableFeedback: properties.getBool('enable-feedback'),
      landscapeLayout: properties.getValue<BottomNavigationBarLandscapeLayout>(
        'landscape-layout',
      ),
    );
    return IrNativeValue(Value(bottomNavigationBarWidget));
  });
}
