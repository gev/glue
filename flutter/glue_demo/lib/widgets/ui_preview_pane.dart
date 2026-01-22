import 'package:flutter/material.dart';

/// Right pane widget containing the UI preview and error display
class UiPreviewPane extends StatelessWidget {
  final Widget? renderedWidget;
  final String? errorMessage;

  const UiPreviewPane({super.key, this.renderedWidget, this.errorMessage});

  @override
  Widget build(BuildContext context) {
    return Container(
      color: Theme.of(context).colorScheme.surface,
      child: Column(
        children: [
          Container(
            padding: const EdgeInsets.all(8),
            color: Theme.of(context).colorScheme.primaryContainer,
            child: Text(
              'Live UI Preview',
              style: Theme.of(context).textTheme.titleMedium,
            ),
          ),
          Expanded(
            child: Container(
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
                          const SizedBox(height: 8),
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
                  : renderedWidget ??
                        const Center(child: Text('No UI to display')),
            ),
          ),
        ],
      ),
    );
  }
}
