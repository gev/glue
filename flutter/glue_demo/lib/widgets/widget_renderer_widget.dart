import 'package:flutter/material.dart';

/// Widget for displaying rendered Glue widgets
class WidgetRendererWidget extends StatelessWidget {
  final Widget? renderedWidget;

  const WidgetRendererWidget({super.key, this.renderedWidget});

  @override
  Widget build(BuildContext context) {
    return Center(child: renderedWidget ?? Text('No UI to display'));
  }
}
