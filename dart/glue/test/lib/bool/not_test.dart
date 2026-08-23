import 'package:glue/src/either.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/bool/not.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Either<EvalError, Ir?> runCode(List<Ir> args) {
  final env = emptyEnv(); // Empty env for unit tests
  final runtime = Runtime.initial(env);

  final evalResult = runEval(apply(not, args), runtime);
  return evalResult.match((error) => Left(error), (value) {
    final (result, _) = value;
    return Right(result);
  });
}

void main() {
  group('Glue.Lib.Bool.Not (Test not function)', () {
    group('Logical not', () {
      test('returns true for false', () {
        final args = [IrBool(false)];
        final result = runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(true))),
        );
      });

      test('returns false for true', () {
        final args = [IrBool(true)];
        final result = runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrBool(false))),
        );
      });
    });
  });
}
