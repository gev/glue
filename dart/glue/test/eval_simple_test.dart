import 'package:glue/src/either.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart' hide Env;
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

      expect(result.isRight, isTrue);
      switch (result) {
        case Left(:final value):
          fail('Should not be left: $value');
        case Right(:final value):
          final (resultValue, runtime) = value;
          expect(resultValue, equals(IrInteger(123)));
          expect(runtime.env, equals(env)); // Environment unchanged
          expect(runtime.context, isEmpty);
      }
    });

    test('runEvalSimple evaluates symbols', () async {
      final result = await runEvalSimple(eval(IrSymbol('x')), env);

      expect(result.isRight, isTrue);
      switch (result) {
        case Left(:final value):
          fail('Should not be left: $value');
        case Right(:final value):
          final (resultValue, runtime) = value;
          expect(resultValue, equals(IrInteger(42)));
          expect(runtime.env, equals(env));
          expect(runtime.context, isEmpty);
      }
    });

    test('runEvalSimple evaluates function calls', () async {
      final call = IrList([IrSymbol('add'), IrSymbol('x'), IrInteger(8)]);
      final result = await runEvalSimple(eval(call), env);

      expect(result.isRight, isTrue);
      switch (result) {
        case Left(:final value):
          fail('Should not be left: $value');
        case Right(:final value):
          final (resultValue, runtime) = value;
          expect(resultValue, equals(IrInteger(50))); // 42 + 8 = 50
          expect(runtime.env, equals(env));
          expect(runtime.context, isEmpty);
      }
    });

    test('runEvalSimple handles errors', () async {
      final result = await runEvalSimple(eval(IrSymbol('nonexistent')), env);

      expect(result.isLeft, isTrue);
      switch (result) {
        case Left(:final value):
          expect(value.exception.symbol, equals('unbound-variable'));
        case Right(:final value):
          fail('Should not be right: $value');
      }
    });

    test('runEvalSimple works with custom Eval actions', () async {
      final action = getEnv().map((env) => env.length);
      final result = await runEvalSimple(action, env);

      expect(result.isRight, isTrue);
      switch (result) {
        case Left(:final value):
          fail('Should not be left: $value');
        case Right(:final value):
          final (frameCount, runtime) = value;
          expect(frameCount, equals(1)); // One frame in our test env
          expect(runtime.env, equals(env));
          expect(runtime.context, isEmpty);
      }
    });

    test('runEvalSimple preserves environment state', () async {
      // First evaluation
      final result1 = await runEvalSimple(eval(IrSymbol('x')), env);
      expect(result1.isRight, isTrue);

      // Second evaluation should work with same environment
      final result2 = await runEvalSimple(eval(IrSymbol('y')), env);
      expect(result2.isRight, isTrue);

      switch (result2) {
        case Left(:final value):
          fail('Should not be left: $value');
        case Right(:final value):
          final (resultValue, _) = value;
          expect(resultValue, equals(IrString('hello')));
      }
    });
  });
}
