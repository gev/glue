import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';
import 'glue_widget.dart';

/// Glue Center widget - Flutter implementation of center wrapper
class GlueCenter extends GlueWidget {
  final IrNativeValue child;

  GlueCenter(this.child, {super.properties, super.key});

  @override
  Widget build(BuildContext context) {
    final childWidget = _extractChildWidget();
    if (childWidget == null) {
      return const SizedBox.shrink();
    }

    return Center(child: childWidget);
  }

  Widget? _extractChildWidget() {
    final hostValue = child.value;
    if (hostValue.value is GlueWidget) {
      return hostValue.value as GlueWidget;
    }
    return null;
  }
}
