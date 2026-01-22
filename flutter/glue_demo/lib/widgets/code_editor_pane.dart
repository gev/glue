import 'package:flutter/material.dart';
import 'package:code_forge/code_forge.dart';

/// Left pane widget containing the Glue code editor
class CodeEditorPane extends StatefulWidget {
  final TextEditingController codeController;
  final bool isEvaluating;

  const CodeEditorPane({
    super.key,
    required this.codeController,
    required this.isEvaluating,
  });

  @override
  State<CodeEditorPane> createState() => _CodeEditorPaneState();
}

class _CodeEditorPaneState extends State<CodeEditorPane> {
  @override
  Widget build(BuildContext context) {
    return Expanded(
      child: Container(
        color: Theme.of(context).colorScheme.surface,
        child: Column(
          children: [
            if (widget.isEvaluating)
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
              child: CodeField(
                controller: widget.codeController,
                fontSize: 20,
                fontFamily: 'monospace',
              ),
            ),
          ],
        ),
      ),
    );
  }
}
