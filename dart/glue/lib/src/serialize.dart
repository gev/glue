import 'package:glue/src/ast.dart';

/// Serialize AST to Glue string representation
/// Mirrors Haskell's Glue.Serialize module
String serializeAst(Ast ast) {
  return _go(ast);
}

String _go(Ast ast) {
  return switch (ast) {
    IntegerAst(:final value) => value.toString(),
    FloatAst(:final value) => value.toString(),
    StringAst(:final value) => '"${_escapeString(value)}"',
    SymbolAst(:final value) => value,
    ListAst(:final elements) => '(${elements.map(_go).join(' ')})',
    ObjectAst(:final properties) =>
      '(${properties.entries.map((e) => ':${e.key} ${_go(e.value)}').join(' ')})',
  };
}

String _escapeString(String s) {
  return s
      .replaceAll('\\', '\\\\')
      .replaceAll('"', '\\"')
      .replaceAll('\n', '\\n')
      .replaceAll('\t', '\\t')
      .replaceAll('\r', '\\r');
}
