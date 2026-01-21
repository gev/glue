import 'package:flutter/material.dart';
import 'package:glue_flutter/src/widgets/glue_widget.dart';

/// Glue Padding widget - Flutter implementation of padding wrapper
class GluePadding extends GlueWidget {
  final Widget child;
  final EdgeInsetsGeometry padding;

  const GluePadding({
    required this.child,
    this.padding = EdgeInsets.zero,
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    return Padding(padding: padding, child: child);
  }
}
