import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'reactive_helpers.dart';

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
          'reactive-widget expects 2 arguments: notifier and child-widget',
        ),
      ),
    );
  }

  final notifierIr = args[0];
  final childWidgetIr = args[1];

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

    // Evaluate the child widget
    return eval(childWidgetIr).flatMap((evaluatedChild) {
      // Extract the widget from the evaluated result
      final childWidget = extractWidget(evaluatedChild);

      // Create ListenableBuilder that wraps the child
      final reactiveContainer = ListenableBuilder(
        listenable: notifier,
        builder: (context, _) => childWidget,
      );

      return Eval.pure(IrNativeValue(HostValue(reactiveContainer)));
    });
  });
}
