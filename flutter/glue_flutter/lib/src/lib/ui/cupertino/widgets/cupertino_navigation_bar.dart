import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoNavigationBar widget function
/// Creates Flutter CupertinoNavigationBar from Glue expressions
/// Expects keyword arguments: :leading, :middle, :trailing, :border, :background-color, etc.
final Ir cupertinoNavigationBar = IrNativeFunc(cupertinoNavigationBarImpl);

/// CupertinoNavigationBar implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoNavigationBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoNavigationBar(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoNavigationBar(WidgetProperties.empty()),
};

/// Create CupertinoNavigationBar widget from properties object
Eval<Ir> _createCupertinoNavigationBar(WidgetProperties properties) {
  final navigationBarWidget = CupertinoNavigationBar(
    leading: properties.getWidget('cupertino-navigation-bar-leading'),
    automaticallyImplyLeading:
        properties.getBool(
          'cupertino-navigation-bar-automatically-imply-leading',
        ) ??
        true,
    automaticallyImplyMiddle:
        properties.getBool(
          'cupertino-navigation-bar-automatically-imply-middle',
        ) ??
        true,
    previousPageTitle: properties.getString(
      'cupertino-navigation-bar-previous-page-title',
    ),
    middle: properties.getWidget('cupertino-navigation-bar-middle'),
    trailing: properties.getWidget('cupertino-navigation-bar-trailing'),
    border: properties.getValue('cupertino-navigation-bar-border'),
    backgroundColor: properties.getValue(
      'cupertino-navigation-bar-background-color',
    ),
    brightness: properties.getValue('cupertino-navigation-bar-brightness'),
    padding: properties.getValue('cupertino-navigation-bar-padding'),
    transitionBetweenRoutes:
        properties.getBool(
          'cupertino-navigation-bar-transition-between-routes',
        ) ??
        true,
    heroTag: properties.getValue('cupertino-navigation-bar-hero-tag'),
  );
  return Eval.pure(IrNativeValue(Value(navigationBarWidget)));
}
