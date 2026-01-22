import 'package:flutter/material.dart';

/// Right pane widget containing the UI preview and error display
class UiPreviewPane extends StatelessWidget {
  final Widget? renderedWidget;
  final String? errorMessage;

  const UiPreviewPane({super.key, this.renderedWidget, this.errorMessage});

  @override
  Widget build(BuildContext context) {
    return Expanded(
      child: Container(
        color: Theme.of(context).colorScheme.surface,
        padding: const EdgeInsets.all(16),
        child: errorMessage != null
            ? Container(
                color: Theme.of(context).colorScheme.errorContainer,
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
              )
            : renderedWidget != null
            ? SingleChildScrollView(child: Center(child: renderedWidget))
            : Center(child: Text('No UI to display')),
      ),
    );
  }
}
