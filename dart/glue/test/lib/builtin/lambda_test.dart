import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart' hide Env;
import 'package:glue/src/lib/builtin/lambda.dart';
import 'package:test/test.dart';

void main() {
  group('Lambda Special Form', () {
    late Env env;

    setUp(() {
      // Create environment with some test variables
      env = fromList([('x', IrInteger(10))]);
    });

    test('creates closure with parameters and body', () async {
      final args = [
        IrList([IrSymbol('a'), IrSymbol('b')]),
        IrSymbol('body'),
      ];
      final result = await runEvalSimple(lambda(args), env);

      expect(result.isRight, isTrue);
      result.fold((error) => fail('Should not be left'), (tuple) {
        final (closure, _) = tuple;
        expect(closure, isA<IrClosure>());
        final c = closure as IrClosure;
        expect(c.params, equals(['a', 'b']));
        expect(c.body, equals(IrSymbol('body')));
        // Environment should contain captured variables
        expect(c.env.length, greaterThan(0));
      });
    });

    test('creates closure with no parameters', () async {
      final args = [IrList([]), IrInteger(42)];
      final result = await runEvalSimple(lambda(args), env);

      expect(result.isRight, isTrue);
      result.fold((error) => fail('Should not be left'), (tuple) {
        final (closure, _) = tuple;
        expect(closure, isA<IrClosure>());
        final c = closure as IrClosure;
        expect(c.params, isEmpty);
        expect(c.body, equals(IrInteger(42)));
      });
    });

    test('extractSymbols extracts list of names', () {
      final input = [IrSymbol('a'), IrSymbol('b')];
      final result = extractSymbols(input);
      expect(result.isRight, isTrue);
      result.fold(
        (error) => fail('Should not be left'),
        (symbols) => expect(symbols, equals(['a', 'b'])),
      );
    });

    test('extractSymbols fails if list contains non-symbols', () {
      final input = [IrSymbol('a'), IrInteger(1)];
      final result = extractSymbols(input);
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final args = [
        IrList([IrSymbol('x')]),
      ]; // Missing body
      final result = await runEvalSimple(lambda(args), env);

      expect(result.isLeft, isTrue);
      result.fold(
        (error) =>
            expect(error.exception.symbol, equals('wrong-argument-type')),
        (tuple) => fail('Should not be right'),
      );
    });

    test('fails with non-list as parameters', () async {
      final args = [IrInteger(1), IrSymbol('body')];
      final result = await runEvalSimple(lambda(args), env);

      expect(result.isLeft, isTrue);
      result.fold(
        (error) =>
            expect(error.exception.symbol, equals('wrong-argument-type')),
        (tuple) => fail('Should not be right'),
      );
    });

    test('fails with non-symbols in parameters', () async {
      final args = [
        IrList([IrInteger(1)]),
        IrSymbol('body'),
      ];
      final result = await runEvalSimple(lambda(args), env);

      expect(result.isLeft, isTrue);
      result.fold(
        (error) =>
            expect(error.exception.symbol, equals('wrong-argument-type')),
        (tuple) => fail('Should not be right'),
      );
    });
  });
}
