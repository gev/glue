import 'package:glue/src/ast.dart';
import 'package:glue/src/ir.dart';

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
