import '../../either.dart';
import '../../eval.dart' hide Either, Left, Right;
import '../../eval/error.dart';
import '../../ir.dart';
import '../../eval/exception.dart';

/// Lambda special form implementation
/// Mirrors Haskell Glue.Lib.Builtin.Lambda exactly

/// Lambda special form - creates closures
EvalIR lambda(List<Ir> args) {
  if (args.length != 2) {
    return throwError(wrongArgumentType(['arguments', 'body']));
  }

  final paramsIr = args[0];
  final body = args[1];

  if (paramsIr is! IrList) {
    return throwError(wrongArgumentType(['arguments list', 'body']));
  }

  final paramSymbols = extractSymbols(paramsIr.elements.unlock);
  return paramSymbols.fold(
    (error) => throwError(wrongArgumentType(['symbols in arguments', 'body'])),
    (params) => makeClosure(params, body),
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
EvalIR makeClosure(List<String> params, Ir body) {
  return getEnv().map((env) => IrClosure(params, body, env));
}
