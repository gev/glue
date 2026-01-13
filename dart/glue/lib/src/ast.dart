import 'package:fast_immutable_collections/fast_immutable_collections.dart';

/// Abstract Syntax Tree for Glue language
/// Mirrors the Haskell AST algebraic data type
sealed class Ast {
  const Ast();

  @override
  String toString();

  @override
  bool operator ==(Object other);

  @override
  int get hashCode;
}

/// String literal AST node
class StringAst extends Ast {
  final String value;

  const StringAst(this.value);

  @override
  String toString() => '"$value"';

  @override
  bool operator ==(Object other) => other is StringAst && other.value == value;

  @override
  int get hashCode => value.hashCode;
}

/// Integer literal AST node
class IntegerAst extends Ast {
  final int value;

  const IntegerAst(this.value);

  @override
  String toString() => value.toString();

  @override
  bool operator ==(Object other) => other is IntegerAst && other.value == value;

  @override
  int get hashCode => value.hashCode;
}

/// Float literal AST node
class FloatAst extends Ast {
  final double value;

  const FloatAst(this.value);

  @override
  String toString() => value.toString();

  @override
  bool operator ==(Object other) => other is FloatAst && other.value == value;

  @override
  int get hashCode => value.hashCode;
}

/// Symbol AST node (identifiers, operators)
class SymbolAst extends Ast {
  final String value;

  const SymbolAst(this.value);

  @override
  String toString() => value;

  @override
  bool operator ==(Object other) => other is SymbolAst && other.value == value;

  @override
  int get hashCode => value.hashCode;
}

/// List AST node (function calls, data lists)
class ListAst extends Ast {
  final IList<Ast> elements;

  const ListAst(this.elements);

  @override
  String toString() => '(${elements.map((e) => e.toString()).join(' ')})';

  @override
  bool operator ==(Object other) =>
      other is ListAst && other.elements == elements;

  @override
  int get hashCode => elements.hashCode;
}

/// Object AST node (property maps with keyword keys)
class ObjectAst extends Ast {
  final IMap<String, Ast> properties;

  const ObjectAst(this.properties);

  @override
  String toString() {
    final props = properties.entries
        .map((e) => ':${e.key} ${e.value}')
        .join(' ');
    return '($props)';
  }

  @override
  bool operator ==(Object other) =>
      other is ObjectAst && other.properties == properties;

  @override
  int get hashCode => properties.hashCode;
}
