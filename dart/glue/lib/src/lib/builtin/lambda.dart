import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';

/// Lambda special form implementation
/// Mirrors Haskell Glue.Lib.Builtin.Lambda exactly

/// Lambda special form - creates closures
/// Mirrors Haskell Glue.Lib.Builtin.Lambda.lambda exactly
final Ir lambda = IrSpecial(lambdaImpl);

/// Lambda special form implementation
/// Mirrors Haskell Glue.Lib.Builtin.Lambda.lambda exactly
Eval<Ir> lambdaImpl(List<Ir> args) {
  if (args.length != 2) {
    return throwError(wrongArgumentType(['arguments', 'body']));
  }

  final paramsIr = args[0];
  final body = args[1];

  if (paramsIr is! IrList) {
    return throwError(wrongArgumentType(['arguments list', 'body']));
  }

  final paramSymbols = extractSymbols(paramsIr.elements.unlock);
  return paramSymbols.match(
    (_) => throwError(wrongArgumentType(['arguments', 'body'])),
    (value) => makeClosure(value, body),
  );
}

/// Extract symbols from parameter list
Either<RuntimeException, List<String>> extractSymbols(List<Ir> irs) {
  final symbols = <String>[];
  for (final ir in irs) {
    if (ir is IrSymbol) {
      symbols.add(ir.value);
    } else {
      return Left(expectedListOfSymbols());
    }
  }
  return Right(symbols);
}

/// Create closure with parameters and body (desugars multi-param to nested single-param)
Eval<Ir> makeClosure(List<String> params, Ir body) {
  return getEnv().map((env) => _makeNestedClosure(params, body, env));
}

/// Helper to create nested single-param closures
Ir _makeNestedClosure(List<String> params, Ir body, Env env) {
  return switch (params) {
    [] => body, // No params, just return body
    [final param] => IrClosure(param, body, env), // Single param
    [final param, ...final rest] => IrClosure(
      param,
      _makeNestedClosure(rest, body, env),
      env,
    ), // Nested closure
  };
}
