import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// MenuAnchor widget function
/// Creates Flutter MenuAnchor from Glue (menu-anchor props) expressions
final Ir menuAnchor = IrNativeFunc(menuAnchorImpl);

/// MenuAnchor implementation - takes properties object
Eval<Ir> menuAnchorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createMenuAnchor(
    Properties(properties.unlock),
  ),
  _ => _createMenuAnchor(Properties.empty()),
};

/// Create MenuAnchor widget from properties
Eval<Ir> _createMenuAnchor(Properties properties) {
  final menuAnchorWidget = MenuAnchor(
    controller: properties.menuAnchorController,
    style: properties.menuAnchorStyle,
    alignmentOffset: properties.menuAnchorAlignmentOffset,
    reservedPadding: properties.menuAnchorReservedPadding,
    layerLink: properties.menuAnchorLayerLink,
    clipBehavior: properties.menuAnchorClipBehavior,
    consumeOutsideTap: properties.menuAnchorConsumeOutsideTap,
    onOpen: properties.menuAnchorOnOpen,
    onClose: properties.menuAnchorOnClose,
    crossAxisUnconstrained: properties.menuAnchorCrossAxisUnconstrained,
    useRootOverlay: properties.menuAnchorUseRootOverlay,
    menuChildren: properties.menuAnchorMenuChildren ?? [],
    builder: properties.menuAnchorBuilder,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(menuAnchorWidget)));
}
