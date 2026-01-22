import 'package:flutter/material.dart';
import 'widgets/code_editor_pane.dart';
import 'widgets/ui_preview_pane.dart';
import 'services/glue_evaluator.dart';

void main() {
  runApp(const GlueDemoApp());
}

class GlueDemoApp extends StatelessWidget {
  const GlueDemoApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Glue Demo - Live UI Editor',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.blue),
        useMaterial3: true,
      ),
      home: const GlueDemoHomePage(),
    );
  }
}

class GlueDemoHomePage extends StatefulWidget {
  const GlueDemoHomePage({super.key});

  @override
  State<GlueDemoHomePage> createState() => _GlueDemoHomePageState();
}

class _GlueDemoHomePageState extends State<GlueDemoHomePage> {
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

(text "Hello, Glue!"
  (:color colors.blue)
  (:size 24)
  (:weight font-weight.bold))

;; Try these examples:
;; (button :label "Click me!")
;; (column :children ((text "Item 1") (text "Item 2")))
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
      if (result.isSuccess) {
        renderedWidget = result.widget;
        errorMessage = null;
      } else {
        renderedWidget = null;
        errorMessage = '${result.errorMessage}\n\n${result.stackTrace}';
      }
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Glue Demo - Live UI Editor'),
        backgroundColor: Theme.of(context).colorScheme.primaryContainer,
      ),
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
          Container(width: 1, color: Theme.of(context).dividerColor),

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
