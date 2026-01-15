import 'package:glue/src/either.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/lib/bool/ge.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<EvalError, Ir?>> runCode(List<Ir> args) async {
  final env = emptyEnv(); // Empty env for unit tests
  final runtime = Runtime.initial(env);

  final evalResult = await runEval(ge(args), runtime);
  return evalResult.match((error) => Left(error), (value) {
    final (result, _) = value;
    return Right(result);
  });
}

void main() {
  group('Glue.Lib.Bool.Ge (Test ge function)', () {
    group('Greater than or equal comparison', () {
      test('returns true for larger integer', () async {
        final args = [IrInteger(15), IrInteger(10)];
        final result = await runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(true))),
        );
      });

      test('returns true for equal integers', () async {
        final args = [IrInteger(10), IrInteger(10)];
        final result = await runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(true))),
        );
      });

      test('returns false for smaller integer', () async {
        final args = [IrInteger(5), IrInteger(10)];
        final result = await runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(false))),
        );
      });

      test('works with floats', () async {
        final args = [IrFloat(3.15), IrFloat(3.14)];
        final result = await runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(true))),
        );
      });

      test('fails with wrong number of arguments', () async {
        final args = [IrInteger(15)];
        final result = await runCode(args);
        expect(result.isLeft, isTrue);
      });
    });
  });
}
