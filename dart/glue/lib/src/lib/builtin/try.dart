import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Try special form - exception handling with catch blocks
/// Mirrors Haskell Glue.Lib.Builtin.Try.tryFunc exactly
Eval<Ir> tryFunc(List<Ir> args) {
  if (args.isEmpty) {
    return throwError(wrongArgumentType(['body', 'catch*']));
  }

  final body = args[0];
  final catches = args.sublist(1);

  return getRuntime().flatMap((runtime) {
    return liftIO(runEval(eval(body), runtime)).flatMap((result) {
      return result.match(
        (error) {
          // Handle RuntimeException exactly like Haskell
          final runtimeExc = error.exception;
          final catchHandler = _findCatch(runtimeExc.symbol, catches);

          if (catchHandler != null) {
            return eval(catchHandler).flatMap((callable) {
              // Check if callable like Haskell does
              if (_isCallable(callable)) {
                // Use same payload handling as Haskell: maybe [] (: []) payload
                final List<Ir> payload = runtimeExc.value != null
                    ? [runtimeExc.value!]
                    : [];
                return apply(callable, payload);
              } else {
                return throwError(notCallableObject());
              }
            });
          } else {
            return throwError(runtimeExc);
          }
        },
        (success) {
          final (value, newRuntime) = success;
          return putRuntime(newRuntime).map((_) => value);
        },
      );
    });
  });
}

/// Find the appropriate catch handler for an exception name
Ir? _findCatch(String excSymbol, List<Ir> catches) {
  for (final catchClause in catches) {
    if (catchClause is IrList && catchClause.elements.length == 3) {
      final elements = catchClause.elements;
      if (elements[0] is IrSymbol &&
          (elements[0] as IrSymbol).value == 'catch') {
        final catchType = elements[1];
        final handler = elements[2];

        final catchTypeName = _getSymbolText(catchType);
        if (catchTypeName == excSymbol) {
          return handler;
        }
      }
    }
  }
  return null;
}

/// Extract symbol text from IR
String? _getSymbolText(Ir ir) {
  return switch (ir) {
    IrSymbol(value: final text) => text,
    _ => null,
  };
}

/// Check if an IR value is callable (closure or native function)
/// Mirrors Haskell isCallable
bool _isCallable(Ir ir) {
  return ir is IrClosure || (ir is IrNative && ir.value is NativeFunc);
}
