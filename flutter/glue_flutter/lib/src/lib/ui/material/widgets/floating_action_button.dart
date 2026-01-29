import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// FloatingActionButton widget function
/// Creates Flutter FloatingActionButton from Glue (floating-action-button props) expressions
final Ir floatingActionButton = IrNativeFunc(floatingActionButtonImpl);

/// FloatingActionButton implementation - takes properties object
Eval<Ir> floatingActionButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createFloatingActionButton(
    MaterialProperties(properties.unlock),
  ),
  _ => _createFloatingActionButton(MaterialProperties.empty()),
};

/// Create FloatingActionButton widget from properties
Eval<Ir> _createFloatingActionButton(MaterialProperties properties) {
  // Get runtime and create widget
  return getRuntime().map((runtime) {
    final fabWidget = FloatingActionButton(
      child: properties.child,
      tooltip: properties.tooltip,
      foregroundColor: properties.foregroundColor,
      backgroundColor: properties.color,
      focusColor: properties.focusColor,
      hoverColor: properties.hoverColor,
      splashColor: properties.splashColor,
      heroTag: properties.heroTag ?? const Object(),
      elevation: properties.size, // using size for elevation
      focusElevation: properties.focusElevation,
      hoverElevation: properties.hoverElevation,
      highlightElevation: properties.highlightElevation,
      disabledElevation: properties.disabledElevation,
      mini: properties.mini ?? false,
      shape: properties.shape,
      clipBehavior: properties.clipBehavior,
      isExtended: properties.isExtended,
      autofocus: properties.autofocus,
      materialTapTargetSize: properties.materialTapTargetSize,
      enableFeedback: properties.fabEnableFeedback,
      onPressed: properties.onPress(runtime),
      mouseCursor: properties.mouseCursor,
      focusNode: properties.focusNode,
    );
    return IrNativeValue(Value(fabWidget));
  });
}
