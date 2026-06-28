import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/env.dart';
import 'package:glue/src/eval.dart';

/// Host value wrapper for any host language object
/// Mirrors Haskell Value exactly with getters
class Value {
  final dynamic value;
  final Map<String, Eval<Ir>> getters;

  const Value(this.value, {this.getters = const {}});

  @override
  String toString() => 'Value($value, getters: ${getters.length})';
}

/// Create a host value from any value
/// Mirrors Haskell hostValue exactly
Value hostValue(dynamic value) => Value(value);

/// Create a host value with properties getters
/// Mirrors Haskell hostValueWithProps exactly
Value hostValueWithProps(dynamic value, Map<String, Eval<Ir>> getters) =>
    Value(value, getters: getters);

/// Extract a host value with type safety
/// Mirrors Haskell extractValue exactly
T? extractValue<T>(Value hostValue) => switch (hostValue) {
  Value(value: T v) => v,
  _ => null,
};

/// Intermediate Representation for Glue language execution
/// Mirrors Haskell IR but simplified for Dart (no type parameter m)
sealed class Ir {
  const Ir();
}

/// Primitive IR values
class IrInteger extends Ir {
  final int value;
  const IrInteger(this.value);

  @override
  String toString() => value.toString();

  @override
  bool operator ==(Object other) =>
      identical(this, other) || (other is IrInteger && other.value == value);

  @override
  int get hashCode => value.hashCode;
}

class IrFloat extends Ir {
  final double value;
  const IrFloat(this.value);

  @override
  String toString() => value.toString();

  @override
  bool operator ==(Object other) =>
      identical(this, other) || (other is IrFloat && other.value == value);

  @override
  int get hashCode => value.hashCode;
}

class IrString extends Ir {
  final String value;
  const IrString(this.value);

  @override
  String toString() => '"$value"';

  @override
  bool operator ==(Object other) =>
      identical(this, other) || (other is IrString && other.value == value);

  @override
  int get hashCode => value.hashCode;
}

class IrBool extends Ir {
  final bool value;
  const IrBool(this.value);

  @override
  String toString() => value ? 'true' : 'false';

  @override
  bool operator ==(Object other) =>
      identical(this, other) || (other is IrBool && other.value == value);

  @override
  int get hashCode => value.hashCode;
}

class IrSymbol extends Ir {
  final String value;
  const IrSymbol(this.value);

  @override
  String toString() => value;

  @override
  bool operator ==(Object other) =>
      identical(this, other) || (other is IrSymbol && other.value == value);

  @override
  int get hashCode => value.hashCode;
}

class IrDottedSymbol extends Ir {
  late final List<String> parts;
  final String value;

  IrDottedSymbol(this.value) {
    parts = value.split('.');
  }

  @override
  String toString() => value;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      (other is IrDottedSymbol && value == other.value);

  @override
  int get hashCode => value.hashCode;
}

/// Composite IR values
class IrList extends Ir {
  final IList<Ir> elements;
  IrList(List<Ir> elements) : elements = IList(elements);

  @override
  String toString() => '(${elements.map((e) => e.toString()).join(' ')})';

  @override
  bool operator ==(Object other) =>
      identical(this, other) || (other is IrList && other.elements == elements);

  @override
  int get hashCode => elements.hashCode;
}

class IrObject extends Ir {
  final IMap<String, Ir> properties;
  IrObject(Map<String, Ir> properties) : properties = IMap(properties);

  @override
  String toString() {
    final entries = properties.entries
        .map((entry) => ':${entry.key} ${entry.value}')
        .join(' ');
    return '($entries)';
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      (other is IrObject && other.properties == properties);

  @override
  int get hashCode => properties.hashCode;
}

class IrEvaluable extends Ir {
  final Eval<Ir> Function() func;
  const IrEvaluable(this.func);

  @override
  String toString() => '<evaluable>';

  @override
  bool operator ==(Object other) => identical(this, other);

  @override
  int get hashCode => func.hashCode;
}

/// Special IR values
class IrVoid extends Ir {
  const IrVoid();

  @override
  String toString() => '#<void>';

  @override
  bool operator ==(Object other) => other is IrVoid;

  @override
  int get hashCode => 'void'.hashCode;
}

class IrNativeValue extends Ir {
  final Value value; // Host language value wrapped in Value
  const IrNativeValue(this.value);

  @override
  bool operator ==(Object other) => identical(this, other);

  @override
  int get hashCode => value.hashCode;

  @override
  String toString() => '<host:${value.toString()}>';
}

class IrNativeFunc extends Ir {
  // Single-arg contract for universal currying
  final Eval<Ir> Function(Ir) function;
  const IrNativeFunc(this.function);

  @override
  String toString() => '<native-func>';

  // All NativeFunc instances are equal (like Haskell)
  @override
  bool operator ==(Object other) => identical(this, other);

  @override
  int get hashCode => function.hashCode;
}

class IrSpecial extends Ir {
  final Eval<Ir> Function(List<Ir>) function;
  const IrSpecial(this.function);

  @override
  String toString() => '<special>';

  // All Special instances are equal (like Haskell)
  @override
  bool operator ==(Object other) => identical(this, other);

  @override
  int get hashCode => function.hashCode;
}

class IrClosure extends Ir {
  final List<String> params; // Multiple params like Haskell
  final Ir body;
  final Env env;
  const IrClosure(this.params, this.body, this.env);

  @override
  String toString() => '<closure>';

  // Closures are not comparable (like Haskell)
  @override
  bool operator ==(Object other) => identical(this, other);

  @override
  int get hashCode => Object.hash(params, body, env);
}

/// Helper functions for IR introspection
/// Mirrors Haskell accessor functions

bool isList(Ir ir) => ir is IrList;

int listLength(Ir ir) => ir is IrList ? ir.elements.length : 0;

bool isObject(Ir ir) => ir is IrObject;

int objectSize(Ir ir) => ir is IrObject ? ir.properties.length : 0;

Ir? objectLookup(String key, Ir ir) =>
    ir is IrObject ? ir.properties[key] : null;

bool isSymbol(Ir ir) => ir is IrSymbol || ir is IrDottedSymbol;

String getSymbol(Ir ir) => switch (ir) {
  IrSymbol(:final value) => value,
  IrDottedSymbol(:final parts) => parts.join('.'),
  _ => '',
};

/// Host value utilities
/// Mirrors Haskell isValue and getValueFromIR exactly

bool isValue(Ir ir) => ir is IrNativeValue;

Value? getValueFromIR(Ir ir) => ir is IrNativeValue ? ir.value : null;

/// Check value is truthy or falsy

bool isTruthy(Ir ir) => switch (ir) {
  IrBool(:final value) => value,
  IrVoid() => false,
  IrString(value: "") => false,
  IrList(elements: []) => false,
  _ => true,
};

bool isFalsy(Ir ir) => !isTruthy(ir);
