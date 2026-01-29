import 'package:code_forge/code_forge.dart';
import 'package:flutter/material.dart';

/// Left pane widget containing the Glue code editor
class CodeEditorPane extends StatefulWidget {
  final TextEditingController codeController;

  const CodeEditorPane({super.key, required this.codeController});

  @override
  State<CodeEditorPane> createState() => _CodeEditorPaneState();
}

class _CodeEditorPaneState extends State<CodeEditorPane> {
  late final CodeForgeController _controller;

  @override
  void initState() {
    super.initState();
    _controller = CodeForgeController();
    _controller.text = widget.codeController.text;

    // Sync changes back to the original controller
    _controller.addListener(() {
      widget.codeController.text = _controller.text;
    });
  }

  @override
  void dispose() {
    _controller.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Expanded(
      child: CodeForge(
        controller: _controller,
        textStyle: const TextStyle(
          fontSize: 20,
          fontFamily: 'Courier',
          height: 1.5,
        ),
      ),
    );
  }
}
