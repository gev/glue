import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Lambda special form implementation
/// Mirrors Haskell Glue.Lib.Builtin.Lambda exactly

/// Lambda special form - creates closures
/// Mirrors Haskell Glue.Lib.Builtin.Lambda.lambda exactly
final Ir lambda = IrSpecial(lambdaImpl);

/// Lambda special form implementation
/// Mirrors Haskell Glue.Lib.Builtin.Lambda.lambda exactly
Eval<Ir> lambdaImpl(List<Ir> args) {
  if (args.length < 2) {
    return throwError(wrongArgumentType(['arguments', 'body']));
  }

  final [paramsIr, ...body] = args;

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

/// Create closure with parameters and body
/// Mirrors Haskell makeClosure exactly - stores all params directly
Eval<Ir> makeClosure(List<String> params, List<Ir> body) {
  return getEnv().map((env) => IrClosure(params, body, env));
}
