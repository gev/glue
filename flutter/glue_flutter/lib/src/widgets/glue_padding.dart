import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';
import 'glue_widget.dart';
import '../utils/edge_insets_parser.dart';

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
