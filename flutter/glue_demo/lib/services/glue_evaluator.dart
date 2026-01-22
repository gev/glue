import 'package:glue/ir.dart';
import 'package:glue/lib/builtin.dart';
import 'package:glue/parser.dart';
import 'package:glue/eval.dart';
import 'package:glue/module.dart';
import 'package:glue_flutter/glue_flutter.dart';
import 'package:flutter/material.dart';
import 'package:glue/lib/bool.dart';

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

      // Extract Flutter widget from evaluation result
      print('🎨 Extracting widget from result...');
      final widget = _extractWidgetFromIr(resultIr);
      print('✅ Widget extraction complete: ${widget.runtimeType}');

      print('🎉 Glue evaluation completed successfully!');
      return EvaluationResult.success(widget);
    } catch (e, stackTrace) {
      print('💥 Glue evaluation failed: $e');
      print('📚 Stack trace: $stackTrace');
      return EvaluationResult.error(e.toString(), stackTrace.toString());
    }
  }

  /// Extract Flutter widget from Glue IR evaluation result
  static Widget _extractWidgetFromIr(Ir ir) {
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
}

/// Result of Glue code evaluation
class EvaluationResult {
  final Widget? widget;
  final String? errorMessage;
  final String? stackTrace;
  final bool isSuccess;

  EvaluationResult._({
    this.widget,
    this.errorMessage,
    this.stackTrace,
    required this.isSuccess,
  });

  factory EvaluationResult.success(Widget widget) {
    return EvaluationResult._(widget: widget, isSuccess: true);
  }

  factory EvaluationResult.error(String errorMessage, String stackTrace) {
    return EvaluationResult._(
      errorMessage: errorMessage,
      stackTrace: stackTrace,
      isSuccess: false,
    );
  }
}
