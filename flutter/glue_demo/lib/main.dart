import 'package:flutter/material.dart';
import 'package:code_forge/code_forge.dart';
import 'package:glue/ast.dart';
import 'package:glue/ir.dart';
import 'package:glue/parser.dart';
import 'package:glue/eval.dart';
import 'package:glue/env.dart';
import 'package:glue/either.dart';
import 'package:glue_flutter/glue_flutter.dart';
import 'package:glue_flutter/src/lib/ui/widgets/text.dart' as ui_text;
import 'package:glue_flutter/src/lib/ui/widgets/button.dart' as ui_button;
import 'package:glue_flutter/src/lib/ui/widgets/container.dart' as ui_container;
import 'package:glue_flutter/src/lib/ui/widgets/column.dart' as ui_column;
import 'package:glue_flutter/src/lib/ui/widgets/row.dart' as ui_row;
import 'package:glue_flutter/src/lib/ui/widgets/center.dart' as ui_center;
import 'package:glue_flutter/src/lib/ui/styles/colors.dart' as ui_colors;
import 'package:glue_flutter/src/lib/ui/styles/font_weight.dart'
    as ui_font_weight;
import 'package:glue_flutter/src/lib/ui/styles/text_align.dart'
    as ui_text_align;
import 'package:glue_flutter/src/lib/ui/styles/cross_axis_alignment.dart'
    as ui_cross_axis;
import 'package:glue_flutter/src/lib/ui/styles/main_axis_alignment.dart'
    as ui_main_axis;

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
      // 1. Parse Glue code to AST
      print('📝 Step 1: Parsing Glue code...');
      final parseResult = parseGlue(code.trim());
      final ast = parseResult.match(
        (error) {
          print('❌ Parse error: $error');
          throw Exception('Parse error: $error');
        },
        (ast) {
          print('✅ Parse successful: $ast');
          return ast;
        },
      );

      // 2. Compile AST to IR
      print('🔧 Step 2: Compiling AST to IR...');
      final ir = compile(ast);
      print('✅ Compilation successful: $ir');

      // 3. Create environment with UI functions
      print('🏗️ Step 3: Creating evaluation environment...');
      // Add UI functions directly to environment for demo purposes
      final bindings = <(String, Ir)>[
        ('text', IrNativeFunc(text)),
        ('button', IrNativeFunc(button)),
        ('container', IrNativeFunc(container)),
        ('column', IrNativeFunc(column)),
        ('row', IrNativeFunc(row)),
        ('center', IrNativeFunc(center)),
        ('colors', IrNativeValue(hostValue(colors))),
        ('font-weight', IrNativeValue(hostValue(fontWeight))),
        ('text-align', IrNativeValue(hostValue(textAlign))),
        ('cross-axis-alignment', IrNativeValue(hostValue(crossAxisAlignment))),
        ('main-axis-alignment', IrNativeValue(hostValue(mainAxisAlignment))),
      ];
      final initialEnv = fromFrame(frameFromList(bindings));
      print('✅ Environment created with ${bindings.length} UI bindings');

      // 4. Evaluate IR in the environment
      print('⚡ Step 4: Evaluating IR...');
      final evalResult = await runEvalSimple(eval(ir), initialEnv);

      final (resultIr, _) = evalResult.match(
        (error) {
          print('❌ Evaluation error: $error');
          throw Exception('Evaluation error: $error');
        },
        (result) {
          print('✅ Evaluation successful: $result');
          return result;
        },
      );

      // 5. Extract Flutter widget from evaluation result
      print('🎨 Step 5: Extracting widget from result...');
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
