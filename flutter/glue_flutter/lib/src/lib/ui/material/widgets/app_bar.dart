import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// AppBar widget function
/// Creates Flutter AppBar from Glue (app-bar props) expressions
final Ir appBar = IrNativeFunc(appBarImpl);

/// AppBar implementation - takes properties object
Eval<Ir> appBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createAppBar(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create AppBar widget from properties
Eval<Ir> _createAppBar(WidgetProperties properties) {
  final appBarWidget = AppBar(
    key: properties.key,
    leading: properties.getWidget('leading'),
    automaticallyImplyLeading:
        properties.getBool('automatically-imply-leading') ?? true,
    title: properties.getWidget('title'),
    actions: properties.getWidgets('actions'),
    automaticallyImplyActions:
        properties.getBool('automatically-imply-actions') ?? true,
    flexibleSpace: properties.getWidget('flexible-space'),
    bottom: properties.getValue('bottom') as PreferredSizeWidget?,
    elevation: properties.getDouble('elevation'),
    scrolledUnderElevation: properties.getDouble('scrolled-under-elevation'),
    notificationPredicate:
        properties.getValue('notification-predicate') ??
        defaultScrollNotificationPredicate,
    shadowColor: properties.getColor('shadow-color'),
    surfaceTintColor: properties.getColor('surface-tint-color'),
    shape: properties.getValue('shape'),
    backgroundColor: properties.getColor('color'),
    foregroundColor: properties.getColor('foreground-color'),
    iconTheme: properties.getValue('icon-theme'),
    actionsIconTheme: properties.getValue('actions-icon-theme'),
    primary: properties.getBool('primary') ?? true,
    centerTitle: properties.getBool('center-title'),
    excludeHeaderSemantics:
        properties.getBool('exclude-header-semantics') ?? false,
    titleSpacing: properties.getDouble('title-spacing'),
    toolbarOpacity: properties.getDouble('toolbar-opacity') ?? 1.0,
    bottomOpacity: properties.getDouble('bottom-opacity') ?? 1.0,
    toolbarHeight: properties.getDouble('toolbar-height'),
    leadingWidth: properties.getDouble('leading-width'),
    toolbarTextStyle: properties.getValue('toolbar-text-style'),
    titleTextStyle: properties.getValue('title-text-style'),
    systemOverlayStyle: properties.getValue('system-overlay-style'),
    forceMaterialTransparency:
        properties.getBool('force-material-transparency') ?? false,
    clipBehavior: properties.getValue('clip-behavior'),
    actionsPadding: properties.getValue('actions-padding'),
    animateColor: properties.getBool('animate-color') ?? false,
  );
  return Eval.pure(IrNativeValue(Value(appBarWidget)));
}
