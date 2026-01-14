import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:test/test.dart';

void main() {
  group('Core Expression Evaluation', () {
    late Runtime runtime;

    setUp(() {
      // Create a runtime with some predefined variables
      final env = fromList([
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
      runtime = Runtime.initial(env);
    });

    test('eval literals returns themselves', () async {
      final intResult = await runEval(eval(IrInteger(123)), runtime);
      expect(intResult.isRight, isTrue);
      intResult.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value.$1, equals(IrInteger(123))),
      );

      final stringResult = await runEval(eval(IrString('world')), runtime);
      expect(stringResult.isRight, isTrue);
      stringResult.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value.$1, equals(IrString('world'))),
      );
    });

    test('evalSymbol looks up variables', () async {
      final result = await runEval(evalSymbol('x'), runtime);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value.$1, equals(IrInteger(42))),
      );

      final stringResult = await runEval(evalSymbol('y'), runtime);
      expect(stringResult.isRight, isTrue);
      stringResult.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value.$1, equals(IrString('hello'))),
      );
    });

    test('evalSymbol throws error for unbound variables', () async {
      final result = await runEval(evalSymbol('nonexistent'), runtime);
      expect(result.isLeft, isTrue);
      result.match(
        (error) => expect(error.exception.symbol, equals('unbound-variable')),
        (value) => fail('Should not be right: $value'),
      );
    });

    test('evalList creates literal lists', () async {
      final listIr = IrList([IrInteger(1), IrInteger(2), IrInteger(3)]);
      final result = await runEval(eval(listIr), runtime);
      expect(result.isRight, isTrue);
      result.match((error) => fail('Should not be left: $error'), (value) {
        expect(value.$1, isA<IrList>());
        final list = value.$1 as IrList;
        expect(list.elements.length, equals(3));
        expect(list.elements[0], equals(IrInteger(1)));
        expect(list.elements[1], equals(IrInteger(2)));
        expect(list.elements[2], equals(IrInteger(3)));
      });
    });

    test('evalList evaluates function calls', () async {
      // Test calling the 'add' function: (add x 8) should equal 50
      final callIr = IrList([IrSymbol('add'), IrSymbol('x'), IrInteger(8)]);

      final result = await runEval(eval(callIr), runtime);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value.$1, equals(IrInteger(50))), // 42 + 8 = 50
      );
    });

    test('evalObject evaluates properties', () async {
      final objIr = IrObject({
        'a': IrInteger(1),
        'b': IrSymbol('x'), // Should evaluate to 42
        'c': IrString('literal'),
      });

      final result = await runEval(eval(objIr), runtime);
      expect(result.isRight, isTrue);
      result.match((error) => fail('Should not be left: $error'), (value) {
        expect(value.$1, isA<IrObject>());
        final obj = value.$1 as IrObject;
        expect(obj.properties['a'], equals(IrInteger(1)));
        expect(obj.properties['b'], equals(IrInteger(42))); // x evaluated
        expect(obj.properties['c'], equals(IrString('literal')));
      });
    });

    test('special forms throw special-form error (not implemented)', () async {
      final defCall = IrList([IrSymbol('def'), IrSymbol('z'), IrInteger(100)]);

      final result = await runEval(eval(defCall), runtime);
      expect(result.isLeft, isTrue);
      result.match(
        (error) => expect(error.exception.symbol, equals('special-form')),
        (value) => fail('Should not be right: $value'),
      );
    });

    test('dotted symbols work for simple access', () async {
      // Create an object and bind it
      final obj = IrObject({'nested': IrInteger(99)});
      final envWithObj = defineVar('obj', obj, runtime.env);
      final runtimeWithObj = runtime.copyWith(env: envWithObj);

      // Access obj.nested
      final dottedIr = IrDottedSymbol(['obj', 'nested']);
      final result = await runEval(eval(dottedIr), runtimeWithObj);

      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value.$1, equals(IrInteger(99))),
      );
    });

    test('dotted symbols throw error for missing properties', () async {
      final obj = IrObject({'a': IrInteger(1)});
      final envWithObj = defineVar('obj', obj, runtime.env);
      final runtimeWithObj = runtime.copyWith(env: envWithObj);

      // Try to access obj.missing
      final dottedIr = IrDottedSymbol(['obj', 'missing']);
      final result = await runEval(eval(dottedIr), runtimeWithObj);

      expect(result.isLeft, isTrue);
      result.match(
        (error) => expect(error.exception.symbol, equals('property-not-found')),
        (value) => fail('Should not be right: $value'),
      );
    });

    test('function application works with closures', () async {
      // Create a simple closure: (lambda (a) (+ a 1))
      final closure = IrClosure(
        ['a'],
        IrList([IrSymbol('add'), IrSymbol('a'), IrInteger(1)]),
        runtime.env,
      );

      // Apply it: ((lambda (a) (+ a 1)) 10) should equal 11
      final result = await runEval(apply(closure, [IrInteger(10)]), runtime);

      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value.$1, equals(IrInteger(11))), // 10 + 1 = 11
      );
    });

    test('partial application works', () async {
      // Create a closure: (lambda (a b) (+ a b))
      final closure = IrClosure(
        ['a', 'b'],
        IrList([IrSymbol('add'), IrSymbol('a'), IrSymbol('b')]),
        runtime.env,
      );

      // Partially apply: ((lambda (a b) (+ a b)) 5) should return a closure
      final partialResult = await runEval(
        apply(closure, [IrInteger(5)]),
        runtime,
      );

      expect(partialResult.isRight, isTrue);
      partialResult.match((error) => fail('Should not be left: $error'), (
        value,
      ) {
        expect(value.$1, isA<IrClosure>());
        final partialClosure = value.$1 as IrClosure;
        expect(partialClosure.params, equals(['b'])); // One param left
      });
    });
  });
}
