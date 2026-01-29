import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// AppBar widget function
/// Creates Flutter AppBar from Glue (app-bar props) expressions
final Ir appBar = IrNativeFunc(appBarImpl);

/// AppBar implementation - takes properties object
Eval<Ir> appBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createAppBar(
    MaterialProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create AppBar widget from properties
Eval<Ir> _createAppBar(MaterialProperties properties) {
  final appBarWidget = AppBar(
    leading: properties.child, // leading widget
    automaticallyImplyLeading: properties.automaticallyImplyLeading ?? true,
    title: properties.title,
    actions: properties.actions,
    automaticallyImplyActions: properties.automaticallyImplyActions ?? true,
    flexibleSpace: properties.flexibleSpace,
    bottom: properties.bottomAppBar,
    elevation: properties.size, // using size for elevation
    scrolledUnderElevation: properties.scrolledUnderElevation,
    notificationPredicate:
        properties.notificationPredicate ?? defaultScrollNotificationPredicate,
    shadowColor: properties.shadowColor,
    surfaceTintColor: properties.surfaceTintColor,
    shape: properties.shape,
    backgroundColor: properties.color,
    foregroundColor: properties.foregroundColor,
    iconTheme: properties.iconTheme,
    actionsIconTheme: properties.actionsIconTheme,
    primary: properties.primary ?? true,
    centerTitle: properties.centerTitle,
    excludeHeaderSemantics: properties.excludeHeaderSemantics ?? false,
    titleSpacing: properties.titleSpacing,
    toolbarOpacity: properties.toolbarOpacity ?? 1.0,
    bottomOpacity: properties.bottomOpacity ?? 1.0,
    toolbarHeight: properties.toolbarHeight,
    leadingWidth: properties.leadingWidth,
    toolbarTextStyle: properties.toolbarTextStyle,
    titleTextStyle: properties.titleTextStyle,
    systemOverlayStyle: properties.systemOverlayStyle,
    forceMaterialTransparency: properties.forceMaterialTransparency ?? false,
    clipBehavior: properties.clipBehavior,
    actionsPadding: properties.actionsPadding,
    animateColor: properties.animateColor ?? false,
  );
  return Eval.pure(IrNativeValue(Value(appBarWidget)));
}
