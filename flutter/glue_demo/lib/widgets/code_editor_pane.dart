import 'package:flutter/material.dart';

/// Left pane widget containing the Glue code editor
class CodeEditorPane extends StatelessWidget {
  final TextEditingController codeController;
  final bool isEvaluating;

  const CodeEditorPane({
    super.key,
    required this.codeController,
    required this.isEvaluating,
  });

  @override
  Widget build(BuildContext context) {
    return Expanded(
      child: Container(
        color: Theme.of(context).colorScheme.surface,
        child: Column(
          children: [
            if (isEvaluating)
              Container(
                padding: const EdgeInsets.all(8),
                color: Theme.of(context).colorScheme.primaryContainer,
                child: Row(
                  children: [
                    const Spacer(),
                    const SizedBox(
                      width: 16,
                      height: 16,
                      child: CircularProgressIndicator(strokeWidth: 2),
                    ),
                  ],
                ),
              ),
            Expanded(
              child: TextField(
                controller: codeController,
                maxLines: null,
                expands: true,
                style: const TextStyle(fontFamily: 'monospace', fontSize: 20),
                decoration: const InputDecoration(
                  border: InputBorder.none,
                  contentPadding: EdgeInsets.all(16),
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }
}
