import 'package:glue/ir.dart';
import 'package:glue/lib/builtin.dart';
import 'package:glue/parser.dart';
import 'package:glue/eval.dart';
import 'package:glue/module.dart';
import 'package:glue_flutter/glue_flutter.dart';
import 'package:flutter/material.dart';
import 'package:glue/lib/bool.dart';
import '../models/evaluation_result.dart';

/// Service for evaluating Glue code and converting results to Flutter widgets
class GlueEvaluator {
  /// Evaluate Glue code and return either a Flutter widget or an error message
  static Future<EvaluationResult> evaluateCode(String code) async {
    print('🔄 Starting Glue evaluation for code: "${code.trim()}"');

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
          final env = envFromModules([builtinModule, boolModule, uiModule]);
          print('✅ Environment created with UI module: $uiModule');

          final evalResult = await runEvalSimple(eval(irTree), env);

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

      // Extract Flutter widgets from evaluation result
      print('🎨 Extracting widgets from result...');
      final widgets = _extractWidgetsFromIr(resultIr);
      print('✅ Widget extraction complete: ${widgets.length} widgets');

      print('🎉 Glue evaluation completed successfully!');
      return EvaluationResult.success(widgets);
    } catch (e, stackTrace) {
      print('💥 Glue evaluation failed: $e');
      print('📚 Stack trace: $stackTrace');
      return EvaluationResult.error(e.toString(), stackTrace.toString());
    }
  }

  /// Extract flattened list of Flutter widgets from Glue IR evaluation result
  static List<Widget> _extractWidgetsFromIr(Ir ir) {
    return switch (ir) {
      IrNativeValue(value: final hostValue) => switch (hostValue.value) {
        Widget widget => [widget],
        _ => [
          Container(
            padding: const EdgeInsets.all(16),
            child: Text('Result: ${hostValue.value}'),
          ),
        ],
      },
      IrString(value: final value) => [Text(value)],
      IrInteger(value: final value) => [Text(value.toString())],
      IrFloat(value: final value) => [Text(value.toString())],
      IrBool(value: final value) => [Text(value.toString())],
      IrList(:final elements) =>
        elements
            .expand(
              (item) => _extractWidgetsFromIr(item),
            ) // Flatten recursively
            .toList(),
      IrVoid() => [], // Ignore void values - don't create widgets for them
      _ => [
        Container(
          padding: const EdgeInsets.all(16),
          child: Text('Glue Result: ${ir.toString()}'),
        ),
      ],
    };
  }
}
