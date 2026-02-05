import 'package:glue/ir.dart';

/// Simple key-value store
class Store {
  final Map<Ir, Ir> _store = {};

  /// Put key-value pair in store
  bool put(Ir key, Ir value) {
    if (_store.containsKey(key)) {
      return false;
    }
    _store[key] = value;
    return true;
  }

  /// Get value by key
  Ir? get(Ir key) {
    return _store[key];
  }
}
