import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// AppBar widget function
/// Creates Flutter AppBar from Glue (app-bar props) expressions
final Ir appBar = IrNativeFunc(appBarImpl);

/// AppBar implementation - takes properties object
Eval<Ir> appBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createAppBar(Properties(properties.unlock)),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create AppBar widget from properties
Eval<Ir> _createAppBar(Properties properties) {
  final appBarWidget = AppBar(
    leading: properties.child, // leading widget
    title: properties.title,
    actions: properties.actions,
    elevation: properties.size, // using size for elevation
    backgroundColor: properties.color,
    foregroundColor: properties.foregroundColor,
    shadowColor: properties.shadowColor,
    surfaceTintColor: properties.surfaceTintColor,
    centerTitle: properties.centerTitle,
    titleSpacing: properties.titleSpacing,
    toolbarOpacity: properties.toolbarOpacity ?? 1.0,
    bottomOpacity: properties.bottomOpacity ?? 1.0,
    toolbarHeight: properties.toolbarHeight,
    leadingWidth: properties.leadingWidth,
    primary: properties.primary ?? true,
    excludeHeaderSemantics: properties.excludeHeaderSemantics ?? false,
    clipBehavior: properties.clipBehavior,
  );
  return Eval.pure(IrNativeValue(Value(appBarWidget)));
}
