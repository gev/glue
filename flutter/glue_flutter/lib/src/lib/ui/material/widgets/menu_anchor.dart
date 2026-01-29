import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// MenuAnchor widget function
/// Creates Flutter MenuAnchor from Glue (menu-anchor props) expressions
final Ir menuAnchor = IrNativeFunc(menuAnchorImpl);

/// MenuAnchor implementation - takes properties object
Eval<Ir> menuAnchorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createMenuAnchor(
    WidgetProperties(properties.unlock),
  ),
  _ => _createMenuAnchor(WidgetProperties.empty()),
};

/// Create MenuAnchor widget from properties
Eval<Ir> _createMenuAnchor(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final menuAnchorWidget = MenuAnchor(
      key: properties.key,
      controller: properties.getValue('controller'),
      style: properties.getValue('style'),
      alignmentOffset: properties.getValue('alignment-offset') ?? Offset.zero,
      reservedPadding: properties.getValue('reserved-padding'),
      layerLink: properties.getValue('layer-link'),
      clipBehavior: properties.getValue('clip-behavior') ?? Clip.hardEdge,
      consumeOutsideTap: properties.getBool('consume-outside-tap') ?? false,
      onOpen: properties.getVoidCallback('on-open', runtime),
      onClose: properties.getVoidCallback('on-close', runtime),
      crossAxisUnconstrained:
          properties.getBool('cross-axis-unconstrained') ?? false,
      useRootOverlay: properties.getBool('use-root-overlay') ?? false,
      menuChildren: properties.getWidgets('menu-children') ?? [],
      builder: properties.getValue('builder'),
      child: properties.child,
    );
    return IrNativeValue(Value(menuAnchorWidget));
  });
}
