import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:test/test.dart';

void main() {
  group('Eval Monad', () {
    test('Eval.pure creates successful evaluation', () async {
      final eval = Eval.pure(42);
      final runtime = Runtime.initial(fromList([]));

      final result = await runEval(eval, runtime);

      expect(result.isRight, isTrue);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (result, runtime) = value;
        expect(result, equals(42));
      });
    });

    test('throwError creates failed evaluation', () async {
      final exception = unboundVariable('test');
      final expectedError = EvalError([], exception);
      final eval = throwError<int>(exception);
      final runtime = Runtime.initial(fromList([]));

      final result = await runEval(eval, runtime);

      expect(result.isLeft, isTrue);
      result.match(
        (error) => expect(error, equals(expectedError)),
        (value) => fail('Should not be right: $value'),
      );
    });

    test('map transforms successful result', () async {
      final eval = Eval.pure(21).map((x) => x * 2);
      final runtime = Runtime.initial(fromList([]));

      final result = await runEval(eval, runtime);

      expect(result.isRight, isTrue);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (result, runtime) = value;
        expect(result, equals(42));
      });
    });

    test('flatMap chains evaluations', () async {
      final eval = Eval.pure(21).flatMap((x) => Eval.pure(x * 2));
      final runtime = Runtime.initial(fromList([]));

      final result = await runEval(eval, runtime);

      expect(result.isRight, isTrue);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (result, runtime) = value;
        expect(result, equals(42));
      });
    });

    test('Runtime state access functions work', () async {
      final initialEnv = fromList([('x', IrInteger(42))]);
      final runtime = Runtime.initial(initialEnv);

      // Test getEnv
      final getEnvResult = await runEval(getEnv(), runtime);
      expect(getEnvResult.isRight, isTrue);
      getEnvResult.match((error) => fail('Should not be left: $error'), (
        value,
      ) {
        final (env, runtime) = value;
        expect(env, equals(initialEnv));
      });

      // Test push/pop context
      final pushResult = await runEval(pushContext('test'), runtime);
      expect(pushResult.isRight, isTrue);
      late Runtime pushedRuntime;
      pushResult.match((error) => fail('Should not be left: $error'), (value) {
        final (result, runtime) = value;
        pushedRuntime = runtime;
        expect(runtime.context, equals(['test']));
      });

      final popResult = await runEval(popContext(), pushedRuntime);
      expect(popResult.isRight, isTrue);
      popResult.match((error) => fail('Should not be left: $error'), (value) {
        final (result, runtime) = value;
        expect(runtime.context, equals([]));
      });
    });

    test('defineVarEval modifies environment', () async {
      final runtime = Runtime.initial(fromList([]));

      final result = await runEval(defineVarEval('x', IrInteger(42)), runtime);

      expect(result.isRight, isTrue);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (result, runtime) = value;
        expect(runtime.env.length, equals(1));
      });
    });

    test('withEnv temporarily changes environment', () async {
      final originalEnv = fromList([('x', IrInteger(1))]);
      final tempEnv = fromList([('y', IrInteger(2))]);
      final runtime = Runtime.initial(originalEnv);

      final eval = withEnv(tempEnv, getEnv());
      final result = await runEval(eval, runtime);

      expect(result.isRight, isTrue);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (env, runtime) = value;
        // Should get temp environment during execution
        expect(env, equals(tempEnv));
      });
    });
  });
}
