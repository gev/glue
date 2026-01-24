import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/error.dart';
import 'package:glue/runtime.dart';
import 'reactive_helpers.dart';

/// Reactive widget that re-evaluates child expression on each build
class ReactiveWidget extends StatelessWidget {
  final ChangeNotifier notifier;
  final Ir childExpr;
  final Runtime runtime;

  const ReactiveWidget({
    required this.notifier,
    required this.childExpr,
    required this.runtime,
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    // Re-evaluate child expression on each reactive update
    final result = evalSync(childExpr, runtime);
    return extractWidget(result) ?? const SizedBox();
  }
}

/// Synchronous evaluation helper for reactive widgets
/// This is a simplified version - in practice, we'd need async evaluation
/// For now, we'll evaluate simple expressions synchronously
Ir evalSync(Ir expression, Runtime runtime) {
  // For widget creation expressions, try to evaluate synchronously
  // This is a placeholder - real implementation would need proper sync evaluation
  switch (expression) {
    case IrList(elements: [IrSymbol(value: 'text'), ...]):
      // For text widgets, we could evaluate synchronously
      // But for now, return a placeholder
      return IrNativeValue(
        HostValue(
          Container(
            padding: const EdgeInsets.all(8),
            child: const Text('Reactive Text - TODO: Implement sync eval'),
          ),
        ),
      );
    case IrNativeValue(value: HostValue(value: Widget _)):
      return expression;
    default:
      // For complex expressions, return placeholder
      return IrNativeValue(
        HostValue(
          Container(
            padding: const EdgeInsets.all(8),
            child: Text('Reactive Widget: ${expression.toString()}'),
          ),
        ),
      );
  }
}

/// Creates a reactive widget that rebuilds when dependencies change
/// Takes a notifier (ChangeNotifier) and a single child widget, returns ListenableBuilder
final reactiveWidget = IrSpecial(reactiveWidgetImpl);

/// Reactive widget special form implementation
Eval<Ir> reactiveWidgetImpl(List<Ir> args) {
  if (args.length != 2) {
    return throwError(
      RuntimeException(
        'wrong-number-of-arguments',
        IrString(
          'reactive-widget expects 2 arguments: notifier and child-expression',
        ),
      ),
    );
  }

  final notifierIr = args[0];
  final childExpr = args[1]; // Store expression, don't evaluate yet

  // Evaluate the notifier argument to get the actual counter object
  return eval(notifierIr).flatMap((evaluatedNotifier) {
    // Extract the ChangeNotifier from the evaluated IrNativeValue
    final notifier = extractChangeNotifier(evaluatedNotifier);
    if (notifier == null) {
      return throwError(
        RuntimeException(
          'wrong-argument-type',
          IrString('first argument must be a ChangeNotifier'),
        ),
      );
    }

    // Get current runtime for dynamic evaluation
    return getRuntime().map((runtime) {
      // Create reactive widget that re-evaluates child on each build
      final reactiveContainer = ReactiveWidget(
        notifier: notifier,
        childExpr: childExpr,
        runtime: runtime,
      );

      return IrNativeValue(HostValue(reactiveContainer));
    });
  });
}
