import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// AlertDialog widget function
/// Creates Flutter AlertDialog from Glue (alert-dialog props) expressions
final Ir alertDialog = IrNativeFunc(alertDialogImpl);

/// AlertDialog implementation - takes properties object
Eval<Ir> alertDialogImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createAlertDialog(
    MaterialProperties(properties.unlock),
  ),
  _ => _createAlertDialog(MaterialProperties.empty()),
};

/// Create AlertDialog widget from properties
Eval<Ir> _createAlertDialog(MaterialProperties properties) {
  final alertDialogWidget = AlertDialog(
    icon: properties.alertDialogIcon,
    iconPadding: properties.alertDialogIconPadding,
    iconColor: properties.alertDialogIconColor,
    title: properties.title,
    titlePadding: properties.alertDialogTitlePadding,
    titleTextStyle: properties.alertDialogTitleTextStyle,
    content: properties.child,
    contentPadding: properties.alertDialogContentPadding,
    contentTextStyle: properties.alertDialogContentTextStyle,
    actions: properties.alertDialogActions,
    actionsPadding: properties.alertDialogActionsPadding,
    actionsAlignment: properties.alertDialogActionsAlignment,
    actionsOverflowAlignment: properties.alertDialogActionsOverflowAlignment,
    actionsOverflowDirection: properties.alertDialogActionsOverflowDirection,
    actionsOverflowButtonSpacing:
        properties.alertDialogActionsOverflowButtonSpacing,
    buttonPadding: properties.alertDialogButtonPadding,
    backgroundColor: properties.color,
    elevation: properties.dividerThickness, // using thickness for elevation
    shadowColor: properties.shadowColor,
    surfaceTintColor: properties.surfaceTintColor,
    semanticLabel: properties.alertDialogSemanticLabel,
    insetPadding: properties.alertDialogInsetPadding,
    clipBehavior: properties.clipBehavior,
    shape: properties.shape,
    alignment: properties.alignment,
    scrollable: properties.alertDialogScrollable,
  );
  return Eval.pure(IrNativeValue(Value(alertDialogWidget)));
}
