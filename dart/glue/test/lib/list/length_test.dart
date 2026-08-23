import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/length.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Length (length)', () {
    test('returns 0 for empty list', () {
      final args = [IrList([])];
      final result = runEvalSimple(apply(length, args), emptyEnv());
      result.match((error) => fail('Length failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrInteger(0)));
      });
    });

    test('returns correct length for non-empty list', () {
      final args = [
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = runEvalSimple(apply(length, args), emptyEnv());
      result.match((error) => fail('Length failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrInteger(3)));
      });
    });

    test('returns length for list with mixed types', () {
      final args = [
        IrList([IrInteger(42), IrString('hello'), IrFloat(3.14)]),
      ];
      final result = runEvalSimple(apply(length, args), emptyEnv());
      result.match((error) => fail('Length failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrInteger(3)));
      });
    });

    test('fails on non-list', () {
      final args = [IrInteger(42)];
      final result = runEvalSimple(apply(length, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
