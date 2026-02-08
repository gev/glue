import 'package:flutter/material.dart';
import 'package:glue_demo/code.dart';
import 'package:glue_demo/services/glue_evaluator.dart';
import 'package:glue_demo/widgets/code_editor_pane.dart';
import 'package:glue_demo/widgets/ui_preview_pane.dart';

class GlueDemo extends StatefulWidget {
  const GlueDemo({super.key});

  @override
  State<GlueDemo> createState() => _GlueDemoState();
}

class _GlueDemoState extends State<GlueDemo> {
  late final TextEditingController codeController;

  List<Widget> renderedWidgets = [];
  String? errorMessage;

  @override
  void initState() {
    super.initState();
    codeController = TextEditingController(text: defaultCode);
    codeController.addListener(_onCodeChanged);
    _evaluateCode(defaultCode);
  }

  @override
  void dispose() {
    codeController.dispose();
    super.dispose();
  }

  void _onCodeChanged() {
    final code = codeController.text;
    if (code.isNotEmpty) {
      _evaluateCode(code);
    }
  }

  Future<void> _evaluateCode(String code) async {
    final result = await GlueEvaluator.evaluateCode(code);
    setState(() {
      result.match(
        (error) {
          renderedWidgets = [];
          errorMessage = error.pretty();
        },
        (widgets) {
          renderedWidgets = widgets;
          errorMessage = null;
        },
      );
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: Row(
        children: [
          // Left panel: Code editor
          Expanded(
            flex: 1,
            child: CodeEditorPane(codeController: codeController),
          ),

          // Right panel: UI renderer
          Expanded(
            flex: 1,
            child: UiPreviewPane(
              renderedWidgets: renderedWidgets,
              errorMessage: errorMessage,
            ),
          ),
        ],
      ),
    );
  }
}
