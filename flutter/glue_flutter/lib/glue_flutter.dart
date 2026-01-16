/// Flutter bindings for the Glue programming language.
///
/// This package provides the ability to execute Glue code within Flutter applications
/// and create Flutter widgets from Glue expressions.
///
/// ## Basic Usage
///
/// ```dart
/// import 'package:glue_flutter/glue_flutter.dart';
///
/// // Create a Glue evaluator
/// final evaluator = GlueEvaluator();
///
/// // Evaluate Glue code that creates widgets
/// final result = await evaluator.evaluate('(text "Hello from Glue!")');
/// ```
library glue_flutter;

export 'src/glue_bindings.dart';
export 'src/widgets/glue_widget.dart';
export 'src/widgets/glue_text.dart';
export 'src/widgets/glue_button.dart';
export 'src/widgets/glue_container.dart';
