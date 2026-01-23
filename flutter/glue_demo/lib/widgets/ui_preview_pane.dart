import 'package:flutter/material.dart';
import 'error_display_widget.dart';
import 'widget_renderer_widget.dart';

/// Right pane widget containing the UI preview and error display
/// Coordinates between ErrorDisplayWidget and WidgetRendererWidget
class UiPreviewPane extends StatelessWidget {
  final List<Widget> renderedWidgets;
  final String? errorMessage;

  const UiPreviewPane({
    required this.renderedWidgets,
    this.errorMessage,
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    return errorMessage != null
        ? ErrorDisplayWidget(errorMessage: errorMessage)
        : WidgetRendererWidget(renderedWidgets: renderedWidgets);
  }
}
