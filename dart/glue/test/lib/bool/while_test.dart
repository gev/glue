import 'package:glue/src/either.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/lib/bool/while.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<EvalError, Ir?>> runCode(List<Ir> args) async {
  final env = emptyEnv(); // Empty env for unit tests
  final runtime = Runtime.initial(env);

  final evalResult = await runEval(while_(args), runtime);
  return evalResult.match((error) => Left(error), (value) {
    final (result, _) = value;
    return Right(result);
  });
}

void main() {
  group('Glue.Lib.Bool.While (Test while special form)', () {
    group('Loop execution', () {
      test('returns void when condition is initially false', () async {
        final args = [IrBool(false), IrInteger(42)];
        final result = await runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrVoid())),
        );
      });

      test('fails with no arguments', () async {
        final args = <Ir>[];
        final result = await runCode(args);
        expect(result.isLeft, isTrue);
      });
    });
  });
}
