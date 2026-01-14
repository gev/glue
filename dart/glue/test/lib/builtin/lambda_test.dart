import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';
import 'package:glue/src/lib/builtin/lambda.dart';

void main() {
  group('Lambda Special Form', () {
    group('Creating closures', () {
      test('creates a closure with parameters and body', () async {
        final initialEnv = fromList([('x', IrInteger(10))]);
        final testRuntime = Runtime.initial(initialEnv);
        final args = [
          IrList([IrSymbol('a'), IrSymbol('b')]),
          IrSymbol('body'),
        ];

        final result = await runEval(lambda(args), testRuntime);

        expect(result.isRight, isTrue);
        result.match((error) => fail('Lambda failed: $error'), (value) {
          final (res, runtime) = value;
          expect(res, isA<IrClosure>());
          final closure = res as IrClosure;
          expect(closure.params, equals(['a', 'b']));
          expect(closure.body, equals(IrSymbol('body')));
          // Check that environment was captured
          final lookupResult = lookupVar('x', closure.env);
          expect(lookupResult.isRight, isTrue);
          lookupResult.match(
            (error) => fail('Lookup failed: $error'),
            (val) => expect(val, equals(IrInteger(10))),
          );
        });
      });

      test('creates a closure with no parameters', () async {
        final args = [IrList([]), IrInteger(42)];
        final runtime = Runtime.initial(fromList([]));

        final result = await runEval(lambda(args), runtime);

        expect(result.isRight, isTrue);
        result.match((error) => fail('Lambda failed: $error'), (value) {
          final (res, runtime) = value;
          expect(res, isA<IrClosure>());
          final closure = res as IrClosure;
          expect(closure.params, isEmpty);
          expect(closure.body, equals(IrInteger(42)));
        });
      });
    });

    group('extractSymbols', () {
      test('extracts symbols from list', () {
        final irs = [IrSymbol('a'), IrSymbol('b')];
        final result = extractSymbols(irs);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(['a', 'b'])),
        );
      });

      test('fails on non-symbols', () {
        final irs = [IrSymbol('a'), IrInteger(1)];
        final result = extractSymbols(irs);
        result.match(
          (error) => expect(error.symbol, equals('expected-list-of-symbols')),
          (value) => fail('Should not be right: $value'),
        );
      });
    });

    group('Error cases', () {
      test('fails with wrong number of arguments', () async {
        final args = [
          IrList([IrSymbol('x')]),
        ];
        final runtime = Runtime.initial(fromList([]));
        final result = await runEval(lambda(args), runtime);
        expect(result.isLeft, isTrue);
      });

      test('fails with non-list as parameters', () async {
        final args = [IrInteger(1), IrSymbol('body')];
        final runtime = Runtime.initial(fromList([]));
        final result = await runEval(lambda(args), runtime);
        expect(result.isLeft, isTrue);
      });

      test('fails with non-symbols in parameters', () async {
        final args = [
          IrList([IrInteger(1)]),
          IrSymbol('body'),
        ];
        final runtime = Runtime.initial(fromList([]));
        final result = await runEval(lambda(args), runtime);
        expect(result.isLeft, isTrue);
      });
    });
  });
}
