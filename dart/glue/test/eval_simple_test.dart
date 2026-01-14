import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:test/test.dart';

void main() {
  group('Simple Evaluation Interface', () {
    late Env env;

    setUp(() {
      // Create environment with some test variables
      env = fromList([
        ('x', IrInteger(42)),
        ('y', IrString('hello')),
        (
          'add',
          IrNative(
            NativeFunc((List<Ir> args) {
              if (args.length == 2 &&
                  args[0] is IrInteger &&
                  args[1] is IrInteger) {
                final a = (args[0] as IrInteger).value;
                final b = (args[1] as IrInteger).value;
                return Eval.pure(IrInteger(a + b));
              }
              return throwError(
                RuntimeException(
                  'type-error',
                  IrString('Expected two integers'),
                ),
              );
            }),
          ),
        ),
      ]);
    });

    test('runEvalSimple evaluates literals', () async {
      final result = await runEvalSimple(eval(IrInteger(123)), env);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (resultValue, runtime) = value;
        expect(resultValue, equals(IrInteger(123)));
      });
    });

    test('runEvalSimple evaluates symbols', () async {
      final result = await runEvalSimple(eval(IrSymbol('x')), env);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (resultValue, runtime) = value;
        expect(resultValue, equals(IrInteger(42)));
      });
    });

    test('runEvalSimple evaluates function calls', () async {
      final call = IrList([IrSymbol('add'), IrSymbol('x'), IrInteger(8)]);
      final result = await runEvalSimple(eval(call), env);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (resultValue, runtime) = value;
        expect(resultValue, equals(IrInteger(50))); // 42 + 8 = 50
      });
    });

    test('runEvalSimple handles errors', () async {
      final result = await runEvalSimple(eval(IrSymbol('nonexistent')), env);

      expect(result.isLeft, isTrue);
      result.match(
        (error) => expect(error.exception.symbol, equals('unbound-variable')),
        (value) => fail('Should not be right: $value'),
      );
    });

    test('runEvalSimple works with custom Eval actions', () async {
      final action = getEnv().map((env) => env.length);
      final result = await runEvalSimple(action, env);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (frameCount, runtime) = value;
        expect(frameCount, equals(1)); // One frame in environment
      });
    });

    test('runEvalSimple preserves environment state', () async {
      // First evaluation
      final result1 = await runEvalSimple(eval(IrSymbol('x')), env);
      expect(result1.isRight, isTrue);

      // Second evaluation should work with same environment
      final result2 = await runEvalSimple(eval(IrSymbol('y')), env);
      expect(result2.isRight, isTrue);

      result2.match((error) => fail('Should not be left: $error'), (value) {
        final (resultValue, _) = value;
        expect(resultValue, equals(IrString('hello')));
      });
    });
  });
}
