import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// BottomNavigationBar widget function
/// Creates Flutter BottomNavigationBar from Glue (bottom-navigation-bar props) expressions
final Ir bottomNavigationBar = IrNativeFunc(bottomNavigationBarImpl);

/// BottomNavigationBar implementation - takes properties object
Eval<Ir> bottomNavigationBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBottomNavigationBar(
    MaterialProperties(properties.unlock),
  ),
  _ => _createBottomNavigationBar(MaterialProperties.empty()),
};

/// Create BottomNavigationBar widget from properties
Eval<Ir> _createBottomNavigationBar(MaterialProperties properties) {
  final bottomNavigationBarWidget = BottomNavigationBar(
    items: properties.bottomNavigationBarItems ?? [],
    onTap: properties.onBottomNavigationBarTap,
    currentIndex: properties.bottomNavigationBarCurrentIndex,
    elevation: properties.bottomNavigationBarElevation,
    type: properties.bottomNavigationBarType,
    fixedColor: properties.bottomNavigationBarFixedColor,
    backgroundColor: properties.bottomNavigationBarBackgroundColor,
    iconSize: properties.bottomNavigationBarIconSize,
    selectedItemColor: properties.bottomNavigationBarSelectedItemColor,
    unselectedItemColor: properties.bottomNavigationBarUnselectedItemColor,
    selectedIconTheme: properties.bottomNavigationBarSelectedIconTheme,
    unselectedIconTheme: properties.bottomNavigationBarUnselectedIconTheme,
    selectedLabelStyle: properties.bottomNavigationBarSelectedLabelStyle,
    unselectedLabelStyle: properties.bottomNavigationBarUnselectedLabelStyle,
    selectedFontSize: properties.bottomNavigationBarSelectedFontSize,
    unselectedFontSize: properties.bottomNavigationBarUnselectedFontSize,
    showSelectedLabels: properties.bottomNavigationBarShowSelectedLabels,
    showUnselectedLabels: properties.bottomNavigationBarShowUnselectedLabels,
    enableFeedback: properties.bottomNavigationBarEnableFeedback,
    landscapeLayout: properties.bottomNavigationBarLandscapeLayout,
  );
  return Eval.pure(IrNativeValue(Value(bottomNavigationBarWidget)));
}
