import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'lambda.dart' show extractSymbols, makeClosure;

/// Def special form implementation
/// Mirrors Haskell Glue.Lib.Builtin.Def exactly

/// Def special form - defines variables and functions
Eval<Ir> def(List<Ir> args) {
  if (args.length < 2) {
    return throwError(wrongArgumentType(['symbol', 'value']));
  }

  final first = args[0];
  final rest = args.sublist(1);

  if (first is IrSymbol) {
    // Simple variable definition: (def symbol value)
    if (rest.length != 1) {
      return throwError(wrongArgumentType(['symbol', 'value']));
    }
    final value = rest[0];
    return eval(value).flatMap((evaluated) {
      return defineVarEval(first.value, evaluated).map((_) => IrVoid());
    });
  } else if (first is IrList) {
    // Function definition shorthand: (def (symbol params...) body...)
    if (first.elements.isEmpty) {
      return throwError(wrongArgumentType(['function signature', 'body']));
    }

    final funcName = first.elements[0];
    final params = first.elements.sublist(1);

    if (funcName is! IrSymbol) {
      return throwError(wrongArgumentType(['function name symbol']));
    }

    // Extract parameter symbols
    final paramSymbols = extractSymbols(params.unlock);
    return paramSymbols.match(
      (_) => throwError(wrongArgumentType(['symbols in function parameters'])),
      (value) {
        // Create body expression
        final body = rest.isEmpty
            ? IrVoid()
            : rest.length == 1
            ? rest[0]
            : IrList(rest);

        // Create closure and define it
        return makeClosure(value, body).flatMap((closure) {
          return defineVarEval(funcName.value, closure).map((_) => closure);
        });
      },
    );
  } else {
    return throwError(
      wrongArgumentType(['symbol or function signature', 'value']),
    );
  }
}
