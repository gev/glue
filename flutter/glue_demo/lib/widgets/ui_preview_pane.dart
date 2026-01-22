import 'package:flutter/material.dart';
import 'error_display_widget.dart';
import 'widget_renderer_widget.dart';

/// Right pane widget containing the UI preview and error display
/// Coordinates between ErrorDisplayWidget and WidgetRendererWidget
class UiPreviewPane extends StatelessWidget {
  final Widget? renderedWidget;
  final String? errorMessage;

  const UiPreviewPane({super.key, this.renderedWidget, this.errorMessage});

  @override
  Widget build(BuildContext context) {
    return Expanded(
      child: errorMessage != null
          ? ErrorDisplayWidget(errorMessage: errorMessage)
          : WidgetRendererWidget(renderedWidget: renderedWidget),
    );
  }
}
