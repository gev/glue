import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/error.dart';
import '../../reactive_helpers.dart';
import 'reactive_widget.dart';

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
