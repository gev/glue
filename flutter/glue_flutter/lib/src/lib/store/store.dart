import 'package:glue/ir.dart';

/// Hierarchical key-value store supporting both flat and nested access
class Store {
  final Map<Ir, Ir> _store = {};

  /// Put key-value pair in store (flat access, key must not exist)
  bool put(Ir key, Ir value) {
    if (_store.containsKey(key)) {
      return false;
    }
    _store[key] = value;
    return true;
  }

  /// Put key-value pair in store (flat access, allows overwriting)
  void set(Ir key, Ir value) {
    _store[key] = value;
  }

  /// Get value by key (flat access)
  Ir? get(Ir key) {
    return _store[key];
  }

  /// Get value by hierarchical path
  Ir? getByPath(List<Ir> path) {
    if (path.isEmpty) return IrVoid();

    final head = path[0];
    final tail = path.sublist(1);

    final nextValue = _store[head];

    return switch ((nextValue, tail)) {
      (Store s, []) => IrNativeValue(Value(s)), // Return nested store
      (Store s, _) => s.getByPath(tail), // Continue traversal
      (_, []) => nextValue, // Return found value
      _ => null, // Path not found
    };
  }

  /// Put value by hierarchical path
  bool putByPath(List<Ir> path, Ir value) {
    if (path.isEmpty) return false;

    final head = path[0];
    final tail = path.sublist(1);

    if (tail.isEmpty) {
      // Final segment - check if key already exists
      if (_store.containsKey(head)) return false;
      _store[head] = value;
      return true;
    } else {
      // Intermediate segment - ensure Store exists and recurse
      final nextValue = _store[head];
      Store nextStore;

      switch (nextValue) {
        case Store s:
          nextStore = s;
        case null:
          nextStore = Store();
          _store[head] = IrNativeValue(Value(nextStore));
        case _:
          return false; // Path conflict - value exists where store needed
      }

      return nextStore.putByPath(tail, value);
    }
  }

  void putKey(Ir keyOrPath, Ir value) {
    switch (keyOrPath) {
      case IrList(elements: final path):
        putByPath(path.unlock, value);
      case _:
        set(keyOrPath, value);
    }
  }

  /// Get value by key (handles both Ir keys and IrList paths)
  Ir getKey(Ir keyOrPath) {
    switch (keyOrPath) {
      case IrList(elements: final path):
        return getByPath(path.unlock) ?? IrVoid();
      case _:
        return get(keyOrPath) ?? IrVoid();
    }
  }
}
