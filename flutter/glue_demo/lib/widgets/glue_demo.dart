import 'package:flutter/material.dart';
import 'package:glue_demo/services/glue_evaluator.dart';
import 'package:glue_demo/widgets/code_editor_pane.dart';
import 'package:glue_demo/widgets/ui_preview_pane.dart';

class GlueDemo extends StatefulWidget {
  const GlueDemo({super.key});

  @override
  State<GlueDemo> createState() => _GlueDemoState();
}

class _GlueDemoState extends State<GlueDemo> {
  // Code editor content
  late final TextEditingController codeController;

  // UI rendering state
  Widget? renderedWidget;
  String? errorMessage;
  bool isEvaluating = false;

  // Default demo code
  static const String defaultCode = '''
;; Welcome to Glue Demo!
;; Edit this code and see the UI update in real-time
(
  (def 
    (hello message) 
    (text :content message
          :color colors.blue
          :size 24
          :weight font-weight.bold))
  (column 
    :children (
        (hello "Hello World!")
        (hello "Hello Glue!")
    ))
)
''';

  @override
  void initState() {
    super.initState();
    codeController = TextEditingController(text: defaultCode);

    // Auto-evaluate on code changes
    codeController.addListener(_onCodeChanged);

    // Initial evaluation
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
    setState(() {
      isEvaluating = true;
      errorMessage = null;
    });

    final result = await GlueEvaluator.evaluateCode(code);

    setState(() {
      isEvaluating = false;
      result.match(
        (error) {
          // Handle Glue errors with meaningful messages
          renderedWidget = null;
          errorMessage = error.pretty(); // Meaningful Glue error message
        },
        (widgets) {
          // Success - compose the flattened list of widgets
          renderedWidget = Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: widgets, // No padding - flat continuous display
          );
          errorMessage = null;
        },
      );
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Glue Demo. Live UI Editor')),
      body: Row(
        children: [
          // Left panel: Code editor
          Expanded(
            flex: 1,
            child: CodeEditorPane(
              codeController: codeController,
              isEvaluating: isEvaluating,
            ),
          ),

          // Divider
          Container(width: 1),

          // Right panel: UI renderer
          Expanded(
            flex: 1,
            child: UiPreviewPane(
              renderedWidget: renderedWidget,
              errorMessage: errorMessage,
            ),
          ),
        ],
      ),
    );
  }
}
