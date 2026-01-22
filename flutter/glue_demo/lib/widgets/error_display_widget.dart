import 'package:flutter/material.dart';

/// Widget for displaying evaluation errors
class ErrorDisplayWidget extends StatelessWidget {
  final String? errorMessage;

  const ErrorDisplayWidget({super.key, this.errorMessage});

  @override
  Widget build(BuildContext context) {
    if (errorMessage == null) return const SizedBox.shrink();

    return Container(
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
    );
  }
}
