import 'package:flutter/foundation.dart';
import 'package:glue/ir.dart';

/// Reactive state notifier that extends ChangeNotifier for Flutter reactivity
class StateNotifier extends ChangeNotifier {
  Ir _value;
  StateNotifier(this._value);
  Ir get value => _value;
  set value(Ir newValue) {
    _value = newValue;
    notifyListeners();
  }
}
