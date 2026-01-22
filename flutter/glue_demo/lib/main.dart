import 'package:flutter/material.dart';
import 'package:code_forge/code_forge.dart';
import 'package:glue/ir.dart';
import 'package:glue/parser.dart';
import 'package:glue/eval.dart';
import 'package:glue/runtime.dart';
import 'package:glue/module.dart';
import 'package:glue_flutter/glue_flutter.dart';

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
    print('🔄 Starting Glue evaluation for code: "${code.trim()}"');

    setState(() {
      isEvaluating = true;
      errorMessage = null;
    });

    try {
      // Follow runCode pattern from dart/glue/test/eval_test.dart
      final parseResult = parseGlue(code.trim());
      final result = parseResult.match(
        (parseError) => throw Exception('Parse error: $parseError'),
        (ast) async {
          print('✅ Parse successful: $ast');

          final irTree = compile(ast);
          print('✅ Compilation successful: $irTree');

          // Create environment with UI module (following stdlib pattern)
          final env = envFromModules([ui]);
          print('✅ Environment created with UI module: $ui');

          final runtime = Runtime.initial(env);
          final evalResult = await runEval(eval(irTree), runtime);

          return evalResult.match(
            (error) => throw Exception('Evaluation error: $error'),
            (value) {
              final (resultIr, _) = value;
              print('✅ Evaluation successful: $resultIr');
              return resultIr;
            },
          );
        },
      );

      final resultIr = await result;

      // Extract Flutter widget from evaluation result
      print('🎨 Extracting widget from result...');
      final widget = _extractWidgetFromIr(resultIr);
      print('✅ Widget extraction complete: ${widget.runtimeType}');

      setState(() {
        renderedWidget = widget;
        isEvaluating = false;
      });

      print('🎉 Glue evaluation completed successfully!');
    } catch (e, stackTrace) {
      print('💥 Glue evaluation failed: $e');
      print('📚 Stack trace: $stackTrace');

      setState(() {
        errorMessage =
            'Glue evaluation failed: ${e.toString()}\n\nStack trace:\n$stackTrace';
        renderedWidget = null;
        isEvaluating = false;
      });
    }
  }

  /// Extract Flutter widget from Glue IR evaluation result
  Widget _extractWidgetFromIr(Ir ir) {
    return switch (ir) {
      IrNativeValue(value: final hostValue) => switch (hostValue.value) {
        Widget widget => widget,
        _ => Container(
          padding: const EdgeInsets.all(16),
          child: Text('Result: ${hostValue.value}'),
        ),
      },
      IrString(value: final value) => Text(value),
      IrInteger(value: final value) => Text(value.toString()),
      IrFloat(value: final value) => Text(value.toString()),
      IrBool(value: final value) => Text(value.toString()),
      _ => Container(
        padding: const EdgeInsets.all(16),
        child: Text('Glue Result: ${ir.toString()}'),
      ),
    };
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
