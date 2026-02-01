import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// AlertDialog widget function
/// Creates Flutter AlertDialog from Glue (alert-dialog props) expressions
final Ir alertDialog = IrNativeFunc(alertDialogImpl);

/// AlertDialog implementation - takes properties object
Eval<Ir> alertDialogImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createAlertDialog(
    WidgetProperties(properties.unlock),
  ),
  _ => _createAlertDialog(WidgetProperties.empty()),
};

/// Create AlertDialog widget from properties
Eval<Ir> _createAlertDialog(WidgetProperties properties) {
  final alertDialogWidget = AlertDialog(
    key: properties.key,
    icon: properties.getWidget('icon'),
    iconPadding: properties.getValue<EdgeInsetsGeometry>('icon-padding'),
    iconColor: properties.getColor('icon-color'),
    title: properties.getWidget('title'),
    titlePadding: properties.getValue<EdgeInsetsGeometry>('title-padding'),
    titleTextStyle: properties.getValue<TextStyle>('title-text-style'),
    content: properties.getWidget('content'),
    contentPadding: properties.getValue<EdgeInsetsGeometry>('content-padding'),
    contentTextStyle: properties.getValue<TextStyle>('content-text-style'),
    actions: properties.getWidgets('actions'),
    actionsPadding: properties.getValue<EdgeInsetsGeometry>('actions-padding'),
    actionsAlignment: properties.getValue<MainAxisAlignment>(
      'actions-alignment',
    ),
    actionsOverflowAlignment: properties.getValue<OverflowBarAlignment>(
      'actions-overflow-alignment',
    ),
    actionsOverflowDirection: properties.getValue<VerticalDirection>(
      'actions-overflow-direction',
    ),
    actionsOverflowButtonSpacing: properties.getDouble(
      'actions-overflow-button-spacing',
    ),
    buttonPadding: properties.getValue<EdgeInsetsGeometry>('button-padding'),
    backgroundColor: properties.getColor('background-color'),
    elevation: properties.getDouble('elevation'),
    shadowColor: properties.getColor('shadow-color'),
    surfaceTintColor: properties.getColor('surface-tint-color'),
    semanticLabel: properties.getString('semantic-label'),
    insetPadding: properties.getValue<EdgeInsets>('inset-padding'),
    clipBehavior: properties.getValue<Clip>('clip-behavior'),
    shape: properties.getValue<ShapeBorder>('shape'),
    alignment: properties.getValue<AlignmentGeometry>('alignment'),
    constraints: properties.getValue<BoxConstraints>('constraints'),
    scrollable: properties.getBool('scrollable') ?? false,
  );
  return Eval.pure(IrNativeValue(Value(alertDialogWidget)));
}
