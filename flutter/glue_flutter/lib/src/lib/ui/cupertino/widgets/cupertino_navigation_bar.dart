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
    key: properties.key,
    leading: properties.getWidget('leading'),
    automaticallyImplyLeading:
        properties.getBool('automatically-imply-leading') ?? true,
    automaticallyImplyMiddle:
        properties.getBool('automatically-imply-middle') ?? true,
    previousPageTitle: properties.getString('previous-page-title'),
    middle: properties.getWidget('middle'),
    trailing: properties.getWidget('trailing'),
    border: properties.getValue<Border>('border'),
    backgroundColor: properties.getColor('background-color'),
    brightness: properties.getValue<Brightness>('brightness'),
    padding: properties.getValue<EdgeInsetsDirectional>('padding'),
    transitionBetweenRoutes:
        properties.getBool('transition-between-routes') ?? true,
  );
  return Eval.pure(IrNativeValue(Value(navigationBarWidget)));
}
