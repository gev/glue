import 'package:fast_immutable_collections/fast_immutable_collections.dart';

import 'ast.dart';

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
  bool operator ==(Object other) => other is IrInteger && other.value == value;

  @override
  int get hashCode => value.hashCode;
}

class IrFloat extends Ir {
  final double value;
  const IrFloat(this.value);

  @override
  String toString() => value.toString();

  @override
  bool operator ==(Object other) => other is IrFloat && other.value == value;

  @override
  int get hashCode => value.hashCode;
}

class IrString extends Ir {
  final String value;
  const IrString(this.value);

  @override
  String toString() => '"$value"';

  @override
  bool operator ==(Object other) => other is IrString && other.value == value;

  @override
  int get hashCode => value.hashCode;
}

class IrBool extends Ir {
  final bool value;
  const IrBool(this.value);

  @override
  String toString() => value ? 'true' : 'false';

  @override
  bool operator ==(Object other) => other is IrBool && other.value == value;

  @override
  int get hashCode => value.hashCode;
}

class IrSymbol extends Ir {
  final String value;
  const IrSymbol(this.value);

  @override
  String toString() => value;

  @override
  bool operator ==(Object other) => other is IrSymbol && other.value == value;

  @override
  int get hashCode => value.hashCode;
}

class IrDottedSymbol extends Ir {
  final List<String> parts;
  IrDottedSymbol(this.parts);

  @override
  String toString() => parts.join('.');

  @override
  bool operator ==(Object other) =>
      other is IrDottedSymbol && _listsEqual(other.parts, parts);

  @override
  int get hashCode => parts.hashCode;
}

bool _listsEqual(List<String> a, List<String> b) {
  if (a.length != b.length) return false;
  for (int i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}

/// Composite IR values
class IrList extends Ir {
  final IList<Ir> elements;
  IrList(List<Ir> elements) : elements = IList(elements);

  @override
  String toString() => '(${elements.map((e) => e.toString()).join(' ')})';

  @override
  bool operator ==(Object other) =>
      other is IrList && other.elements == elements;

  @override
  int get hashCode => elements.hashCode;
}

class IrObject extends Ir {
  final IMap<String, Ir> properties;
  IrObject(Map<String, Ir> properties) : properties = IMap(properties);

  @override
  String toString() => '{object}';

  @override
  bool operator ==(Object other) =>
      other is IrObject && other.properties == properties;

  @override
  int get hashCode => properties.hashCode;
}

class IrModule extends Ir {
  final IMap<String, Ir> bindings;
  IrModule(Map<String, Ir> bindings) : bindings = IMap(bindings);

  @override
  String toString() => '{module}';

  @override
  bool operator ==(Object other) =>
      other is IrModule && other.bindings == bindings;

  @override
  int get hashCode => bindings.hashCode;
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

class IrNative extends Ir {
  final Native value;
  const IrNative(this.value);

  @override
  String toString() => '<native>';

  @override
  bool operator ==(Object other) => other is IrNative && other.value == value;

  @override
  int get hashCode => value.hashCode;
}

class IrClosure extends Ir {
  final List<String> params;
  final Ir body;
  final Env env;
  const IrClosure(this.params, this.body, this.env);

  @override
  String toString() => '<closure>';

  @override
  bool operator ==(Object other) =>
      other is IrClosure &&
      other.params == params &&
      other.body == body &&
      other.env == env;

  @override
  int get hashCode => Object.hash(params, body, env);
}

/// Native functions and special forms
/// Simplified for Dart (no type parameter m)
sealed class Native {
  const Native();
}

class NativeFunc extends Native {
  // TODO: Type this properly when we implement evaluation
  final dynamic function;
  const NativeFunc(this.function);

  @override
  bool operator ==(Object other) =>
      other is NativeFunc && other.function == function;

  @override
  int get hashCode => function.hashCode;
}

class NativeSpecial extends Native {
  // TODO: Type this properly when we implement evaluation
  final dynamic function;
  const NativeSpecial(this.function);

  @override
  bool operator ==(Object other) =>
      other is NativeSpecial && other.function == function;

  @override
  int get hashCode => function.hashCode;
}

/// Environment types for closures
/// Mirrors Haskell Frame and Env types
typedef Frame = IMap<String, Ir>;
typedef Env = IList<Frame>;

/// Compile AST to IR
/// Mirrors Haskell compile function exactly
Ir compile(Ast ast) {
  return switch (ast) {
    StringAst(:final value) => IrString(value),
    IntegerAst(:final value) => IrInteger(value),
    FloatAst(:final value) => IrFloat(value),
    SymbolAst(:final value) =>
      value.contains('.') ? IrDottedSymbol(value.split('.')) : IrSymbol(value),
    ListAst(:final elements) => IrList(elements.map(compile).toList()),
    ObjectAst(:final properties) => IrObject(
      properties.map((key, value) => MapEntry(key, compile(value))).unlock,
    ),
  };
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
