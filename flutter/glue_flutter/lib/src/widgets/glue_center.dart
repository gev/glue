import 'package:flutter/material.dart';
import 'package:glue_flutter/src/widgets/glue_widget.dart';

/// Glue Center widget - Flutter implementation of center wrapper
class GlueCenter extends GlueWidget {
  final Widget child;

  const GlueCenter({required this.child, super.key});

  @override
  Widget build(BuildContext context) {
    return Center(child: child);
  }
}
