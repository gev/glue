import 'package:glue/src/either.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/lib/bool/eq.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<EvalError, Ir?>> runCode(List<Ir> args) async {
  final env = emptyEnv(); // Empty env for unit tests
  final runtime = Runtime.initial(env);

  final evalResult = await runEval(eq(args), runtime);
  return evalResult.match((error) => Left(error), (value) {
    final (result, _) = value;
    return Right(result);
  });
}

void main() {
  group('Glue.Lib.Bool.Eq (Test eq function)', () {
    group('Equality comparison', () {
      test('returns true for equal numbers', () async {
        final args = [IrInteger(42), IrInteger(42)];
        final result = await runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(true))),
        );
      });

      test('returns false for unequal numbers', () async {
        final args = [IrInteger(42), IrInteger(43)];
        final result = await runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(false))),
        );
      });

      test('returns true for equal strings', () async {
        final args = [IrString('hello'), IrString('hello')];
        final result = await runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(true))),
        );
      });

      test('returns false for unequal strings', () async {
        final args = [IrString('hello'), IrString('world')];
        final result = await runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(false))),
        );
      });

      test('fails with wrong number of arguments', () async {
        final args = [IrInteger(42)];
        final result = await runCode(args);
        expect(result.isLeft, isTrue);
      });
    });
  });
}
