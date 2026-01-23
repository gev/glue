import 'package:flutter/material.dart';

/// Widget for displaying rendered Glue widgets
class WidgetRendererWidget extends StatelessWidget {
  final List<Widget> renderedWidgets;

  const WidgetRendererWidget({super.key, required this.renderedWidgets});

  @override
  Widget build(BuildContext context) {
    return renderedWidgets.isNotEmpty
        ? Column(children: renderedWidgets)
        : Center(child: Text('No UI to display'));
  }
}
