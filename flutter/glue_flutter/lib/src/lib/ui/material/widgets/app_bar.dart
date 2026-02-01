import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// AppBar widget function
/// Creates Flutter AppBar from Glue expressions
/// Expects keyword arguments
final Ir appBar = IrNativeFunc(appBarImpl);

/// AppBar implementation - takes properties object with keyword arguments
Eval<Ir> appBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createAppBar(
    WidgetProperties(properties.unlock),
  ),
  _ => _createAppBar(WidgetProperties.empty()),
};

/// Create AppBar widget from properties object
Eval<Ir> _createAppBar(WidgetProperties properties) {
  final widget = AppBar(
    key: properties.key,
    leading: properties.getWidget('leading'),
    automaticallyImplyLeading:
        properties.getBool('automatically-imply-leading') ?? true,
    title: properties.getWidget('title'),
    actions: properties.getWidgets('actions'),
    automaticallyImplyActions:
        properties.getBool('automatically-imply-actions') ?? true,
    flexibleSpace: properties.getWidget('flexible-space'),
    bottom: properties.getValue<PreferredSizeWidget>('bottom'),
    elevation: properties.getDouble('elevation'),
    scrolledUnderElevation: properties.getDouble('scrolled-under-elevation'),
    notificationPredicate: properties.getValue<>('notification-predicate'),
    shadowColor: properties.getColor('shadow-color'),
    surfaceTintColor: properties.getColor('surface-tint-color'),
    shape: properties.getValue<>('shape'),
    backgroundColor: properties.getColor('background-color'),
    foregroundColor: properties.getColor('foreground-color'),
    iconTheme: properties.getValue<>('icon-theme'),
    actionsIconTheme: properties.getValue<>('actions-icon-theme'),
    primary: properties.getBool('primary') ?? true,
    centerTitle: properties.getBool('center-title'),
    excludeHeaderSemantics:
        properties.getBool('exclude-header-semantics') ?? false,
    titleSpacing: properties.getDouble('title-spacing'),
    toolbarOpacity: properties.getDouble('toolbar-opacity') ?? 1.0,
    bottomOpacity: properties.getDouble('bottom-opacity') ?? 1.0,
    toolbarHeight: properties.getDouble('toolbar-height'),
    leadingWidth: properties.getDouble('leading-width'),
    toolbarTextStyle: properties.getValue<>('toolbar-text-style'),
    titleTextStyle: properties.getValue<>('title-text-style'),
    systemOverlayStyle: properties.getValue<>('system-overlay-style'),
    forceMaterialTransparency:
        properties.getBool('force-material-transparency') ?? false,
    useDefaultSemanticsOrder:
        properties.getBool('use-default-semantics-order') ?? true,
    clipBehavior: properties.getValue<>('clip-behavior') ?? Clip.none,
    actionsPadding: properties.getValue<>('actions-padding'),
    animateColor: properties.getBool('animate-color') ?? true,
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
