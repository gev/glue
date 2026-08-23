import 'package:glue/src/either.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/bool/eq.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Either<EvalError, Ir?> runCode(List<Ir> args) {
  final env = emptyEnv(); // Empty env for unit tests
  final runtime = Runtime.initial(env);

  final evalResult = runEval(apply(eq, args), runtime);
  return evalResult.match((error) => Left(error), (value) {
    final (result, _) = value;
    return Right(result);
  });
}

void main() {
  group('Glue.Lib.Bool.Eq (Test eq function)', () {
    group('Equality comparison', () {
      test('returns true for equal numbers', () {
        final args = [IrInteger(42), IrInteger(42)];
        final result = runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(true))),
        );
      });

      test('returns false for unequal numbers', () {
        final args = [IrInteger(42), IrInteger(43)];
        final result = runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(false))),
        );
      });

      test('returns true for equal strings', () {
        final args = [IrString('hello'), IrString('hello')];
        final result = runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(true))),
        );
      });

      test('returns false for unequal strings', () {
        final args = [IrString('hello'), IrString('world')];
        final result = runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(false))),
        );
      });
    });
  });
}
