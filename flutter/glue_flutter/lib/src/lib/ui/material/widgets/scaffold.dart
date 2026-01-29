import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// Scaffold widget function
/// Creates Flutter Scaffold from Glue (scaffold props) expressions
final Ir scaffold = IrNativeFunc(scaffoldImpl);

/// Scaffold implementation - takes properties object
Eval<Ir> scaffoldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createScaffold(
    MaterialProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Scaffold widget from properties
Eval<Ir> _createScaffold(MaterialProperties properties) {
  final scaffoldWidget = Scaffold(
    appBar: properties.appBar,
    body: properties.body,
    floatingActionButton: properties.floatingActionButton,
    floatingActionButtonLocation: properties.floatingActionButtonLocation,
    floatingActionButtonAnimator: properties.floatingActionButtonAnimatorProper,
    persistentFooterButtons: properties.persistentFooterButtons,
    drawer: properties.drawer,
    endDrawer: properties.endDrawer,
    bottomNavigationBar: properties.bottomNavigationBar,
    bottomSheet: properties.bottomSheet,
    backgroundColor: properties.color,
    resizeToAvoidBottomInset: properties.resizeToAvoidBottomInset,
    primary: properties.primary ?? true,
    drawerDragStartBehavior:
        properties.drawerDragStartBehavior ?? DragStartBehavior.start,
    extendBody: properties.extendBody ?? false,
    extendBodyBehindAppBar: properties.extendBodyBehindAppBar ?? false,
    drawerScrimColor: properties.drawerScrimColor,
    drawerEdgeDragWidth: properties.drawerEdgeDragWidth,
    drawerEnableOpenDragGesture: properties.drawerEnableOpenDragGesture ?? true,
    endDrawerEnableOpenDragGesture:
        properties.endDrawerEnableOpenDragGesture ?? true,
    restorationId: properties.restorationId,
  );
  return Eval.pure(IrNativeValue(Value(scaffoldWidget)));
}
