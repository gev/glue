import 'package:flutter/material.dart';
import 'package:code_forge/code_forge.dart';

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
;; (column :children [(text "Item 1") (text "Item 2")])
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

    // Simulate evaluation delay
    await Future.delayed(const Duration(milliseconds: 500));

    try {
      // Simple pattern matching for demo purposes
      final trimmedCode = code.trim();

      if (trimmedCode.contains('(text')) {
        // Demo text widget
        setState(() {
          renderedWidget = const Text(
            'Hello from Glue!',
            style: TextStyle(
              color: Colors.blue,
              fontSize: 24,
              fontWeight: FontWeight.bold,
            ),
          );
          isEvaluating = false;
        });
      } else if (trimmedCode.contains('(button')) {
        // Demo button widget
        setState(() {
          renderedWidget = ElevatedButton(
            onPressed: () {},
            child: const Text('Demo Button'),
          );
          isEvaluating = false;
        });
      } else if (trimmedCode.contains('(column')) {
        // Demo column widget
        setState(() {
          renderedWidget = const Column(
            children: [
              Text('Item 1'),
              SizedBox(height: 8),
              Text('Item 2'),
              SizedBox(height: 8),
              Text('Item 3'),
            ],
          );
          isEvaluating = false;
        });
      } else {
        // Show the code as text for unrecognized patterns
        setState(() {
          renderedWidget = Container(
            padding: const EdgeInsets.all(16),
            child: Text(
              'Glue Code Preview:\n\n$code',
              style: const TextStyle(fontFamily: 'monospace'),
            ),
          );
          isEvaluating = false;
        });
      }
    } catch (e) {
      setState(() {
        errorMessage = 'Demo evaluation failed: ${e.toString()}';
        renderedWidget = null;
        isEvaluating = false;
      });
    }
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
            child: Container(
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
            ),
          ),

          // Divider
          Container(width: 1, color: Theme.of(context).dividerColor),

          // Right panel: UI renderer
          Expanded(
            flex: 1,
            child: Container(
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
                              color: Theme.of(
                                context,
                              ).colorScheme.errorContainer,
                              padding: const EdgeInsets.all(16),
                              child: Column(
                                crossAxisAlignment: CrossAxisAlignment.start,
                                children: [
                                  Text(
                                    'Evaluation Error:',
                                    style: TextStyle(
                                      color: Theme.of(
                                        context,
                                      ).colorScheme.error,
                                      fontWeight: FontWeight.bold,
                                    ),
                                  ),
                                  const SizedBox(height: 8),
                                  Text(
                                    errorMessage!,
                                    style: TextStyle(
                                      color: Theme.of(
                                        context,
                                      ).colorScheme.error,
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
            ),
          ),
        ],
      ),
    );
  }
}
