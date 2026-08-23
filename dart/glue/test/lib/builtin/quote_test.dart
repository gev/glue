import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/builtin/quote.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';

void main() {
  group('Quote Special Form', () {
    group('Quote returns argument unevaluated', () {
      test('returns integer unevaluated', () {
        final args = [IrInteger(42)];
        final runtime = Runtime.initial(fromList([]));
        final result = runEval(apply(quote, args), runtime);
        result.match((error) => fail('Quote failed: $error'), (value) {
          final (res, _) = value;
          expect(res, equals(IrInteger(42)));
        });
      });

      test('returns symbol unevaluated (not variable lookup)', () {
        final args = [IrSymbol('x')];
        final runtime = Runtime.initial(fromList([]));
        final result = runEval(apply(quote, args), runtime);
        result.match((error) => fail('Quote failed: $error'), (value) {
          final (res, _) = value;
          expect(res, equals(IrSymbol('x')));
        });
      });

      test('returns string unevaluated', () {
        final args = [IrString('hello')];
        final runtime = Runtime.initial(fromList([]));
        final result = runEval(apply(quote, args), runtime);
        result.match((error) => fail('Quote failed: $error'), (value) {
          final (res, _) = value;
          expect(res, equals(IrString('hello')));
        });
      });

      test('returns list unevaluated (not evaluated)', () {
        final args = [
          IrList([IrSymbol('+'), IrInteger(1), IrInteger(2)]),
        ];
        final runtime = Runtime.initial(fromList([]));
        final result = runEval(apply(quote, args), runtime);
        result.match((error) => fail('Quote failed: $error'), (value) {
          final (res, _) = value;
          expect(
            res,
            equals(IrList([IrSymbol('+'), IrInteger(1), IrInteger(2)])),
          );
        });
      });

      test('returns object unevaluated', () {
        final args = [
          IrObject({'name': IrString('Alice')}),
        ];
        final runtime = Runtime.initial(fromList([]));
        final result = runEval(apply(quote, args), runtime);
        result.match((error) => fail('Quote failed: $error'), (value) {
          final (res, _) = value;
          expect(res, equals(IrObject({'name': IrString('Alice')})));
        });
      });
    });

    group('Error cases', () {
      test('fails with multiple arguments', () {
        final args = [IrInteger(1), IrInteger(2)];
        final runtime = Runtime.initial(fromList([]));
        final result = runEval(apply(quote, args), runtime);
        expect(result.isLeft, isTrue);
      });
    });
  });
}
