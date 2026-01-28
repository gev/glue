import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// BottomAppBar widget function
/// Creates Flutter BottomAppBar from Glue (bottom-app-bar props) expressions
final Ir bottomAppBar = IrNativeFunc(bottomAppBarImpl);

/// BottomAppBar implementation - takes properties object
Eval<Ir> bottomAppBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBottomAppBar(
    Properties(properties.unlock),
  ),
  _ => _createBottomAppBar(Properties.empty()),
};

/// Create BottomAppBar widget from properties
Eval<Ir> _createBottomAppBar(Properties properties) {
  final bottomAppBarWidget = BottomAppBar(
    color: properties.bottomAppBarColor,
    elevation: properties.bottomAppBarElevation,
    shape: properties.bottomAppBarShape as NotchedShape?,
    clipBehavior: properties.bottomAppBarClipBehavior,
    notchMargin: properties.bottomAppBarNotchMargin,
    height: properties.bottomAppBarHeight,
    padding: properties.bottomAppBarPadding,
    surfaceTintColor: properties.bottomAppBarSurfaceTintColor,
    shadowColor: properties.bottomAppBarShadowColor,
    child: properties.bottomAppBarChild,
  );
  return Eval.pure(IrNativeValue(Value(bottomAppBarWidget)));
}
