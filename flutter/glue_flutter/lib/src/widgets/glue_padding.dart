import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';
import 'glue_widget.dart';

/// Parse edge insets from Glue IR value
EdgeInsets? parseEdgeInsets(Ir ir) {
  return switch (ir) {
    IrFloat(value: final val) => EdgeInsets.all(val),
    IrInteger(value: final val) => EdgeInsets.all(val.toDouble()),
    IrList(elements: final elements) => _parseEdgeInsetsFromList(elements),
    _ => null,
  };
}

/// Parse edge insets from list
EdgeInsets? _parseEdgeInsetsFromList(dynamic elements) {
  final list = elements.unlock as List<Ir>;
  return switch (list.length) {
    1 => EdgeInsets.all((list[0] as IrFloat).value),
    2 => EdgeInsets.symmetric(
      vertical: (list[0] as IrFloat).value,
      horizontal: (list[1] as IrFloat).value,
    ),
    4 => EdgeInsets.only(
      top: (list[0] as IrFloat).value,
      right: (list[1] as IrFloat).value,
      bottom: (list[2] as IrFloat).value,
      left: (list[3] as IrFloat).value,
    ),
    _ => null,
  };
}

/// Glue Padding widget - Flutter implementation of padding wrapper
class GluePadding extends GlueWidget {
  final IrNativeValue child;
  final Map<String, dynamic> properties;

  const GluePadding(this.child, this.properties);

  @override
  Widget build(BuildContext context) {
    final padding = properties['padding'] != null
        ? parseEdgeInsets(properties['padding']!)
        : EdgeInsets.zero;

    final childWidget = _extractChildWidget();
    if (childWidget == null) {
      return const SizedBox.shrink();
    }

    return Padding(padding: padding ?? EdgeInsets.zero, child: childWidget);
  }

  Widget? _extractChildWidget() {
    final hostValue = child.value;
    if (hostValue.value is GlueWidget) {
      return hostValue.value as GlueWidget;
    }
    return null;
  }
}
