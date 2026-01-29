import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// CupertinoNavigationBar widget function
/// Creates Flutter CupertinoNavigationBar from Glue expressions
/// Expects keyword arguments: :leading, :middle, :trailing, :border, :background-color, etc.
final Ir cupertinoNavigationBar = IrNativeFunc(cupertinoNavigationBarImpl);

/// CupertinoNavigationBar implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoNavigationBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoNavigationBar(
    Properties(properties.unlock),
  ),
  _ => _createCupertinoNavigationBar(Properties.empty()),
};

/// Create CupertinoNavigationBar widget from properties object
Eval<Ir> _createCupertinoNavigationBar(Properties properties) {
  final navigationBarWidget = CupertinoNavigationBar(
    leading: properties.cupertinoNavigationBarLeading,
    automaticallyImplyLeading:
        properties.cupertinoNavigationBarAutomaticallyImplyLeading,
    automaticallyImplyMiddle:
        properties.cupertinoNavigationBarAutomaticallyImplyMiddle,
    previousPageTitle: properties.cupertinoNavigationBarPreviousPageTitle,
    middle: properties.cupertinoNavigationBarMiddle,
    trailing: properties.cupertinoNavigationBarTrailing,
    border: properties.cupertinoNavigationBarBorder,
    backgroundColor: properties.cupertinoNavigationBarBackgroundColor,
    brightness: properties.cupertinoNavigationBarBrightness,
    padding: properties.cupertinoNavigationBarPadding,
    transitionBetweenRoutes:
        properties.cupertinoNavigationBarTransitionBetweenRoutes,
    heroTag: properties.cupertinoNavigationBarHeroTag,
  );
  return Eval.pure(IrNativeValue(Value(navigationBarWidget)));
}
