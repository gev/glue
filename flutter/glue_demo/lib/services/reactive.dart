import 'package:flutter/foundation.dart';
import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Reactive counter that extends ChangeNotifier for Flutter reactivity
class ReactiveCounter extends ChangeNotifier {
  int _value;

  ReactiveCounter(this._value);

  int get value => _value;

  set value(int newValue) {
    _value = newValue;
    notifyListeners();
  }

  void increment(int amount) {
    value = _value + amount;
  }

  void decrement(int amount) {
    value = _value - amount;
  }
}

/// Creates a reactive counter with HostValue getters and setters
/// Returns IrNativeValue(HostValue(ReactiveCounter))
final reactiveCounter = IrNativeFunc((Ir initialValue) {
  final initial = initialValue is IrInteger ? initialValue.value : 0;
  final counter = ReactiveCounter(initial);

  return Eval.pure(
    IrNativeValue(
      HostValue(
        counter,
        getters: {
          'value': Eval(
            (runtime) => Right((IrInteger(counter.value), runtime)),
          ),
          'increment': Eval(
            (runtime) => Right((
              IrNativeFunc((Ir amount) {
                final amt = amount is IrInteger ? amount.value : 1;
                counter.increment(amt);
                return Eval.pure(IrVoid());
              }),
              runtime,
            )),
          ),
          'decrement': Eval(
            (runtime) => Right((
              IrNativeFunc((Ir amount) {
                final amt = amount is IrInteger ? amount.value : 1;
                counter.decrement(amt);
                return Eval.pure(IrVoid());
              }),
              runtime,
            )),
          ),
        },
        setters: {
          'value': (Ir newValue) => Eval((runtime) {
            counter.value = (newValue as IrInteger).value;
            return Right((IrVoid(), runtime));
          }),
        },
      ),
    ),
  );
});

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
    final notifier = _extractChangeNotifier(evaluatedNotifier);
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
      final childWidget = _extractWidget(evaluatedChild);

      // Create ListenableBuilder that wraps the child
      final reactiveContainer = ListenableBuilder(
        listenable: notifier,
        builder: (context, _) => childWidget,
      );

      return Eval.pure(IrNativeValue(HostValue(reactiveContainer)));
    });
  });
}

/// Helper function to extract ChangeNotifier from IrNativeValue
ChangeNotifier? _extractChangeNotifier(Ir ir) {
  if (ir is IrNativeValue) {
    final hostValue = ir.value;
    final actualValue = hostValue.value;
    return actualValue is ChangeNotifier ? actualValue as ChangeNotifier : null;
  }
  return null;
}

/// Helper function to extract a single widget from Ir
Widget _extractWidget(Ir ir) {
  if (ir is IrNativeValue) {
    final hostValue = ir.value;
    final actualValue = hostValue.value;
    if (actualValue is Widget) {
      return actualValue;
    }
  }
  return const Text('Invalid widget');
}

/// Helper function to extract list of widgets from Ir
/// This needs to evaluate each widget expression first
List<Widget> _extractWidgetList(Ir ir) {
  if (ir is! IrList) return [];

  // For now, return empty list - we need to evaluate widget expressions
  // The child widgets need to be evaluated in the context where they can access variables
  print(
    'Widget list extraction not implemented yet - need to evaluate widget expressions',
  );
  return [const Text('Widgets not implemented yet')];
}
