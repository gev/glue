import 'package:flutter/foundation.dart';

/// Reactive counter that extends ChangeNotifier for Flutter reactivity
class CounterNotifier extends ChangeNotifier {
  int _value;

  CounterNotifier(this._value);

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
