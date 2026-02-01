import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Scaffold widget function
/// Creates Flutter Scaffold from Glue (scaffold props) expressions
final Ir scaffold = IrNativeFunc(scaffoldImpl);

/// Scaffold implementation - takes properties object
Eval<Ir> scaffoldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createScaffold(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Scaffold widget from properties
Eval<Ir> _createScaffold(WidgetProperties properties) {
  final scaffoldWidget = Scaffold(
    key: properties.key,
    appBar: properties.getValue<>('app-bar'),
    body: properties.getWidget('body'),
    floatingActionButton: properties.getWidget('floating-action-button'),
    floatingActionButtonLocation: properties.getValue<>(
      'floating-action-button-location',
    ),
    floatingActionButtonAnimator: properties.getValue<>(
      'floating-action-button-animator',
    ),
    persistentFooterButtons: properties.getWidgets('persistent-footer-buttons'),
    drawer: properties.getWidget('drawer'),
    endDrawer: properties.getWidget('end-drawer'),
    bottomNavigationBar: properties.getWidget('bottom-navigation-bar'),
    bottomSheet: properties.getWidget('bottom-sheet'),
    backgroundColor: properties.getColor('background-color'),
    resizeToAvoidBottomInset: properties.getBool(
      'resize-to-avoid-bottom-inset',
    ),
    primary: properties.getBool('primary') ?? true,
    drawerDragStartBehavior:
        properties.getValue<>('drawer-drag-start-behavior') ??
        DragStartBehavior.start,
    extendBody: properties.getBool('extend-body') ?? false,
    extendBodyBehindAppBar:
        properties.getBool('extend-body-behind-app-bar') ?? false,
    drawerScrimColor: properties.getColor('drawer-scrim-color'),
    drawerEdgeDragWidth: properties.getDouble('drawer-edge-drag-width'),
    drawerEnableOpenDragGesture:
        properties.getBool('drawer-enable-open-drag-gesture') ?? true,
    endDrawerEnableOpenDragGesture:
        properties.getBool('end-drawer-enable-open-drag-gesture') ?? true,
    restorationId: properties.getString('restoration-id'),
  );
  return Eval.pure(IrNativeValue(Value(scaffoldWidget)));
}
