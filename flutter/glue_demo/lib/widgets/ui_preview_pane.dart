import 'package:flutter/material.dart';

/// Right pane widget containing the UI preview and error display
class UiPreviewPane extends StatelessWidget {
  final Widget? renderedWidget;
  final String? errorMessage;

  const UiPreviewPane({super.key, this.renderedWidget, this.errorMessage});

  @override
  Widget build(BuildContext context) {
    return errorMessage != null
        ? Container(
            color: Theme.of(context).colorScheme.errorContainer,
            child: SingleChildScrollView(
              padding: const EdgeInsets.all(16),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    'Evaluation Error:',
                    style: TextStyle(
                      color: Theme.of(context).colorScheme.error,
                      fontWeight: FontWeight.bold,
                    ),
                  ),
                  SizedBox(height: 8),
                  Text(
                    errorMessage!,
                    style: TextStyle(
                      color: Theme.of(context).colorScheme.error,
                      fontFamily: 'monospace',
                    ),
                  ),
                ],
              ),
            ),
          )
        : Center(child: renderedWidget ?? Text('No UI to display'));
  }
}
