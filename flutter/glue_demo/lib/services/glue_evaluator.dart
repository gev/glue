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
      final widgets = _extractWidgetsFromIr(resultIr, <Widget>[]);
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
  static List<Widget> _extractWidgetsFromIr(Ir ir, List<Widget> accum) {
    switch (ir) {
      case IrNativeValue(value: final hostValue):
        switch (hostValue.value) {
          case Widget widget:
            accum.add(widget);
          default:
            accum.add(
              Container(
                padding: const EdgeInsets.all(16),
                child: Text('Result: ${hostValue.value}'),
              ),
            );
        }
      case IrString(value: final value):
        accum.add(Text(value));
      case IrInteger(value: final value):
        accum.add(Text(value.toString()));
      case IrFloat(value: final value):
        accum.add(Text(value.toString()));
      case IrBool(value: final value):
        accum.add(Text(value.toString()));
      case IrList(:final elements):
        for (final element in elements) {
          _extractWidgetsFromIr(element, accum); // Recursive flattening
        }
      case IrVoid():
      // Ignore void values - don't add anything
      default:
        accum.add(
          Container(
            padding: const EdgeInsets.all(16),
            child: Text('Glue Result: ${ir.toString()}'),
          ),
        );
    }
    return accum;
  }
}
