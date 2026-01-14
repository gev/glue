import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/lib/builtin/lambda.dart';
import 'package:test/test.dart';

void main() {
  group('Lambda Special Form', () {
    late Runtime runtime;

    setUp(() {
      final env = fromList([
        ('x', IrInteger(42)),
        ('y', IrString('hello')),
        (
          '+',
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
      runtime = Runtime.initial(env);
    });

    test('lambda creates closure with correct parameters and body', () async {
      final lambdaIr = IrList([
        IrSymbol('lambda'),
        IrList([IrSymbol('a'), IrSymbol('b')]),
        IrList([IrSymbol('+'), IrSymbol('a'), IrSymbol('b')]),
      ]);

      final result = await runEval(eval(lambdaIr), runtime);
      expect(result.isRight, isTrue);

      result.match((error) => fail('Should not be left: $error'), (value) {
        expect(value.$1, isA<IrClosure>());
        final closure = value.$1 as IrClosure;
        expect(closure.params, equals(['a', 'b']));
        expect(closure.body, isA<IrList>());
      });
    });

    test('lambda closure captures environment', () async {
      final lambdaIr = IrList([
        IrSymbol('lambda'),
        IrList([IrSymbol('z')]),
        IrList([IrSymbol('+'), IrSymbol('x'), IrSymbol('z')]),
      ]);

      final result = await runEval(eval(lambdaIr), runtime);
      expect(result.isRight, isTrue);

      result.match((error) => fail('Should not be left: $error'), (
        value,
      ) async {
        final closure = value.$1 as IrClosure;
        // Apply the closure: (closure 8) should equal 42 + 8 = 50
        final applyResult = await runEval(
          apply(closure, [IrInteger(8)]),
          runtime,
        );

        expect(applyResult.isRight, isTrue);
        applyResult.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value.$1, equals(IrInteger(50))), // 42 + 8
        );
      });
    });

    test('lambda with empty parameter list', () async {
      final lambdaIr = IrList([IrSymbol('lambda'), IrList([]), IrInteger(123)]);

      final result = await runEval(eval(lambdaIr), runtime);
      expect(result.isRight, isTrue);

      result.match((error) => fail('Should not be left: $error'), (
        value,
      ) async {
        final closure = value.$1 as IrClosure;
        expect(closure.params, isEmpty);

        // Apply the closure: (closure) should equal 123
        final applyResult = await runEval(apply(closure, []), runtime);
        expect(applyResult.isRight, isTrue);
        applyResult.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value.$1, equals(IrInteger(123))),
        );
      });
    });

    test('lambda with single parameter', () async {
      final lambdaIr = IrList([
        IrSymbol('lambda'),
        IrList([IrSymbol('x')]),
        IrList([IrSymbol('+'), IrSymbol('x'), IrInteger(10)]),
      ]);

      final result = await runEval(eval(lambdaIr), runtime);
      expect(result.isRight, isTrue);

      result.match((error) => fail('Should not be left: $error'), (
        value,
      ) async {
        final closure = value.$1 as IrClosure;
        expect(closure.params, equals(['x']));

        // Apply the closure: (closure 5) should equal 5 + 10 = 15
        final applyResult = await runEval(
          apply(closure, [IrInteger(5)]),
          runtime,
        );

        expect(applyResult.isRight, isTrue);
        applyResult.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value.$1, equals(IrInteger(15))), // 5 + 10
        );
      });
    });

    test('lambda parameter validation', () {
      final paramSymbols = [IrSymbol('a'), IrSymbol('b'), IrInteger(123)];

      final result = extractSymbols(paramSymbols);
      result.match(
        (error) => expect(error.symbol, equals('expected-list-of-symbols')),
        (value) => fail('Should not be right: $value'),
      );
    });

    test('lambda parameter validation success', () {
      final paramSymbols = [IrSymbol('a'), IrSymbol('b'), IrSymbol('c')];

      final result = extractSymbols(paramSymbols);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(['a', 'b', 'c'])),
      );
    });

    test('lambda evaluation with invalid syntax', () async {
      // Lambda with too few arguments
      final invalidLambda = IrList([
        IrSymbol('lambda'),
        IrList([IrSymbol('x')]),
        // Missing body
      ]);

      final result = await runEval(eval(invalidLambda), runtime);
      expect(result.isLeft, isTrue);

      result.match(
        (error) =>
            expect(error.exception.symbol, equals('wrong-argument-type')),
        (value) => fail('Should not be right: $value'),
      );
    });
  });
}
