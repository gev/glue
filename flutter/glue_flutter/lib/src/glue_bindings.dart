import 'package:flutter/material.dart';

/// Core Glue evaluator for Flutter integration.
///
/// This class provides the bridge between Glue code execution and Flutter widgets.
/// It manages the Glue environment and provides Flutter-specific functions.
class GlueEvaluator {
  /// The underlying Glue runtime environment
  late final glue.Env _env;

  /// Creates a new Glue evaluator with Flutter bindings
  GlueEvaluator() {
    _env = glue.Env.create();
    _setupFlutterBindings();
  }

  /// Evaluates Glue code and returns the result
  ///
  /// [code] The Glue code to evaluate
  /// Returns the evaluation result, which may be a Flutter widget
  Future<dynamic> evaluate(String code) async {
    try {
      final result = glue.evalString(code, _env);
      return _convertToFlutterType(result);
    } catch (e) {
      throw GlueEvaluationException('Failed to evaluate Glue code: $e');
    }
  }

  /// Sets up Flutter-specific bindings in the Glue environment
  void _setupFlutterBindings() {
    // Add Flutter widget creation functions to the environment
    _env.def('text', glue.NativeFunc(_createTextWidget));
    _env.def('button', glue.NativeFunc(_createButtonWidget));
    _env.def('container', glue.NativeFunc(_createContainerWidget));
  }

  /// Converts Glue values to appropriate Flutter types
  dynamic _convertToFlutterType(dynamic value) {
    if (value is glue.GlueString) {
      return value.value;
    } else if (value is glue.GlueInt) {
      return value.value;
    } else if (value is glue.GlueBool) {
      return value.value;
    }
    return value;
  }

  // Flutter widget creation functions for Glue

  /// Creates a Text widget from Glue arguments
  dynamic _createTextWidget(glue.ListExpr args, glue.Env env) {
    if (args.elements.isEmpty) {
      throw ArgumentError('text requires at least one argument');
    }

    final text = args.elements[0];
    final textValue = text is glue.GlueString ? text.value : text.toString();

    // Parse optional style arguments
    Color? color;
    double? fontSize;

    for (var i = 1; i < args.elements.length; i += 2) {
      if (i + 1 < args.elements.length) {
        final key = args.elements[i];
        final value = args.elements[i + 1];

        if (key is glue.Symbol && key.name == ':color') {
          if (value is glue.GlueString) {
            color = _parseColor(value.value);
          }
        } else if (key is glue.Symbol && key.name == ':size') {
          if (value is glue.GlueInt) {
            fontSize = value.value.toDouble();
          }
        }
      }
    }

    return GlueText(text: textValue, color: color, fontSize: fontSize);
  }

  /// Creates a Button widget from Glue arguments
  dynamic _createButtonWidget(glue.ListExpr args, glue.Env env) {
    // Placeholder - will be implemented with GlueButton
    return GlueButton(label: 'Button', onPressed: () {});
  }

  /// Creates a Container widget from Glue arguments
  dynamic _createContainerWidget(glue.ListExpr args, glue.Env env) {
    // Placeholder - will be implemented with GlueContainer
    return GlueContainer(children: const []);
  }

  /// Parses color strings to Flutter Colors
  Color? _parseColor(String colorStr) {
    // Simple color parsing - can be extended
    switch (colorStr.toLowerCase()) {
      case 'red':
        return Colors.red;
      case 'blue':
        return Colors.blue;
      case 'green':
        return Colors.green;
      case 'black':
        return Colors.black;
      case 'white':
        return Colors.white;
      default:
        return null;
    }
  }
}

/// Exception thrown when Glue code evaluation fails
class GlueEvaluationException implements Exception {
  final String message;

  GlueEvaluationException(this.message);

  @override
  String toString() => 'GlueEvaluationException: $message';
}
