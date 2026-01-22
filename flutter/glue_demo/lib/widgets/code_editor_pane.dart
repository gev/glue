import 'package:flutter/material.dart';
import 'package:code_forge/code_forge.dart';

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
    return Container(
      color: Theme.of(context).colorScheme.surface,
      child: Column(
        children: [
          Container(
            padding: const EdgeInsets.all(8),
            color: Theme.of(context).colorScheme.primaryContainer,
            child: Row(
              children: [
                Text(
                  'Glue Code Editor',
                  style: Theme.of(context).textTheme.titleMedium,
                ),
                const Spacer(),
                if (isEvaluating)
                  const SizedBox(
                    width: 16,
                    height: 16,
                    child: CircularProgressIndicator(strokeWidth: 2),
                  ),
              ],
            ),
          ),
          Expanded(child: CodeForge()),
        ],
      ),
    );
  }
}
