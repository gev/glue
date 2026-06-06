import 'package:glue/either.dart';
import 'package:glue/src/ast.dart';
import 'package:glue/src/ir.dart';

/// Decompile IR to AST (reverse of compile)
/// Returns Either with error for non-serializable IR types
/// Mirrors Haskell decompile function exactly
Either<String, Ast> decompile(Ir ir) {
  return switch (ir) {
    IrInteger(:final value) => Right(IntegerAst(value)),
    IrFloat(:final value) => Right(FloatAst(value)),
    IrString(:final value) => Right(StringAst(value)),
    IrBool(:final value) => Right(SymbolAst(value ? 'true' : 'false')),
    IrSymbol(:final value) => Right(SymbolAst(value)),
    IrDottedSymbol(:final value) => Right(SymbolAst(value)),
    IrList(:final elements) => _sequence(
      elements.map(decompile).toList(),
    ).map((list) => ListAst(list)),
    IrObject(properties: final properties) =>
      _sequence(properties.values.map(decompile).toList()).map((list) {
        final entries = properties.keys.toList();
        final map = <String, Ast>{};
        for (int i = 0; i < entries.length; i++) {
          map[entries[i]] = list[i];
        }
        return ObjectAst(map);
      }),
    IrVoid() => const Left('Cannot decompile Void'),
    IrEvaluable() => const Left('Cannot decompile Evaluable'),
    IrNativeValue() => const Left('Cannot decompile NativeValue'),
    IrNativeFunc() => const Left('Cannot decompile NativeFunc'),
    IrSpecial() => const Left('Cannot decompile Special'),
    IrClosure() => const Left('Cannot decompile Closure'),
  };
}

/// Helper to convert `List<Either>` to `Either<List>`
/// Mirrors Haskell's sequence function
Either<String, List<T>> _sequence<T>(List<Either<String, T>> results) {
  final List<T> acc = [];
  for (final r in results) {
    switch (r) {
      case Left(:final value):
        return Left(value);
      case Right(:final value):
        acc.add(value);
    }
  }
  return Right(acc);
}
