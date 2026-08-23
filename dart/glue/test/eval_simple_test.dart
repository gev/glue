import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
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
          IrNativeFunc(
            (Ir firstArg) => Eval.pure(
              IrNativeFunc((Ir secondArg) {
                if (firstArg is IrInteger && secondArg is IrInteger) {
                  final a = firstArg.value;
                  final b = secondArg.value;
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
        ),
      ]);
    });

    test('runEvalSimple evaluates literals', () {
      final result = runEvalSimple(eval(IrInteger(123)), env);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (resultValue, runtime) = value;
        expect(resultValue, equals(IrInteger(123)));
      });
    });

    test('runEvalSimple evaluates symbols', () {
      final result = runEvalSimple(eval(IrSymbol('x')), env);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (resultValue, runtime) = value;
        expect(resultValue, equals(IrInteger(42)));
      });
    });

    test('runEvalSimple evaluates function calls', () {
      final call = IrList([IrSymbol('add'), IrSymbol('x'), IrInteger(8)]);
      final result = runEvalSimple(eval(call), env);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (resultValue, runtime) = value;
        expect(resultValue, equals(IrInteger(50))); // 42 + 8 = 50
      });
    });

    test('runEvalSimple handles errors', () {
      final result = runEvalSimple(eval(IrSymbol('nonexistent')), env);

      expect(result.isLeft, isTrue);
      result.match(
        (error) => expect(error.exception.symbol, equals('unbound-variable')),
        (value) => fail('Should not be right: $value'),
      );
    });

    test('runEvalSimple works with custom Eval actions', () {
      final action = getEnv().map((env) => env.length);
      final result = runEvalSimple(action, env);
      result.match((error) => fail('Should not be left: $error'), (value) {
        final (frameCount, runtime) = value;
        expect(frameCount, equals(1)); // One frame in environment
      });
    });

    test('runEvalSimple preserves environment state', () {
      // First evaluation
      final result1 = runEvalSimple(eval(IrSymbol('x')), env);
      expect(result1.isRight, isTrue);

      // Second evaluation should work with same environment
      final result2 = runEvalSimple(eval(IrSymbol('y')), env);
      expect(result2.isRight, isTrue);

      result2.match((error) => fail('Should not be left: $error'), (value) {
        final (resultValue, _) = value;
        expect(resultValue, equals(IrString('hello')));
      });
    });
  });
}
