import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart' hide Env;
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
      result.fold((error) => fail('Should not be left'), (tuple) {
        expect(tuple.$1, equals(42));
        expect(tuple.$2, equals(runtime));
      });
    });

    test('throwError creates failed evaluation', () async {
      final exception = unboundVariable('test');
      final expectedError = EvalError([], exception);
      final eval = throwError<int>(exception);
      final runtime = Runtime.initial(fromList([]));

      final result = await runEval(eval, runtime);

      expect(result.isLeft, isTrue);
      result.fold(
        (err) => expect(err, equals(expectedError)),
        (tuple) => fail('Should not be right'),
      );
    });

    test('map transforms successful result', () async {
      final eval = Eval.pure(21).map((x) => x * 2);
      final runtime = Runtime.initial(fromList([]));

      final result = await runEval(eval, runtime);

      expect(result.isRight, isTrue);
      result.fold((error) => fail('Should not be left'), (tuple) {
        expect(tuple.$1, equals(42));
        expect(tuple.$2, equals(runtime));
      });
    });

    test('flatMap chains evaluations', () async {
      final eval = Eval.pure(21).flatMap((x) => Eval.pure(x * 2));
      final runtime = Runtime.initial(fromList([]));

      final result = await runEval(eval, runtime);

      expect(result.isRight, isTrue);
      result.fold((error) => fail('Should not be left'), (tuple) {
        expect(tuple.$1, equals(42));
        expect(tuple.$2, equals(runtime));
      });
    });

    test('Runtime state access functions work', () async {
      final initialEnv = fromList([('x', IrInteger(42))]);
      final runtime = Runtime.initial(initialEnv);

      // Test getEnv
      final getEnvResult = await runEval(getEnv(), runtime);
      expect(getEnvResult.isRight, isTrue);
      getEnvResult.fold(
        (error) => fail('Should not be left'),
        (tuple) => expect(tuple.$1, equals(initialEnv)),
      );

      // Test push/pop context
      final pushResult = await runEval(pushContext('test'), runtime);
      expect(pushResult.isRight, isTrue);
      late Runtime pushedRuntime;
      pushResult.fold((error) => fail('Should not be left'), (tuple) {
        expect(tuple.$2.context, equals(['test']));
        pushedRuntime = tuple.$2;
      });

      final popResult = await runEval(popContext(), pushedRuntime);
      expect(popResult.isRight, isTrue);
      popResult.fold(
        (error) => fail('Should not be left'),
        (tuple) => expect(tuple.$2.context, equals([])),
      );
    });

    test('defineVarEval modifies environment', () async {
      final runtime = Runtime.initial(fromList([]));

      final result = await runEval(defineVarEval('x', IrInteger(42)), runtime);

      expect(result.isRight, isTrue);
      result.fold((error) => fail('Should not be left'), (tuple) {
        expect(tuple.$2.env.length, equals(1));
        final frame = tuple.$2.env[0];
        expect(frame['x'], equals(IrInteger(42)));
      });
    });

    test('withEnv temporarily changes environment', () async {
      final originalEnv = fromList([('x', IrInteger(1))]);
      final tempEnv = fromList([('y', IrInteger(2))]);
      final runtime = Runtime.initial(originalEnv);

      final eval = withEnv(tempEnv, getEnv());
      final result = await runEval(eval, runtime);

      expect(result.isRight, isTrue);
      result.fold((error) => fail('Should not be left'), (tuple) {
        // Should get temp environment during execution
        expect(tuple.$1, equals(tempEnv));
        // Should restore original environment after
        expect(tuple.$2.env, equals(originalEnv));
      });
    });
  });
}
