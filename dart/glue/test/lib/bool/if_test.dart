import 'package:glue/src/either.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/lib/bool/if.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<EvalError, Ir?>> runCode(List<Ir> args) async {
  final env = emptyEnv(); // Empty env for unit tests
  final runtime = Runtime.initial(env);

  final evalResult = await runEval(if_(args), runtime);
  return evalResult.match((error) => Left(error), (value) {
    final (result, _) = value;
    return Right(result);
  });
}

void main() {
  group('Glue.Lib.Bool.If (Test if special form)', () {
    group('Conditional execution', () {
      test('executes then branch when condition is true', () async {
        final args = [IrBool(true), IrInteger(42), IrInteger(0)];
        final result = await runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(42))),
        );
      });

      test('executes else branch when condition is false', () async {
        final args = [IrBool(false), IrInteger(42), IrInteger(0)];
        final result = await runCode(args);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(0))),
        );
      });

      test('fails with wrong number of arguments', () async {
        final args = [IrBool(true), IrInteger(42)];
        final result = await runCode(args);
        expect(result.isLeft, isTrue);
      });
    });
  });
}
