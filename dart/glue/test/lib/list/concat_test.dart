import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/concat.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Concat (concat)', () {
    test('concats two lists', () {
      final args = [
        IrList([IrInteger(1), IrInteger(2)]),
        IrList([IrInteger(3), IrInteger(4)]),
      ];
      final result = runEvalSimple(apply(concat, args), emptyEnv());
      result.match((error) => fail('Concat failed: $error'), (value) {
        final (res, _) = value;
        expect(
          res,
          equals(
            IrList([IrInteger(1), IrInteger(2), IrInteger(3), IrInteger(4)]),
          ),
        );
      });
    });

    test('concats empty list to non-empty list', () {
      final args = [
        IrList([IrInteger(1), IrInteger(2)]),
        IrList([]),
      ];
      final result = runEvalSimple(apply(concat, args), emptyEnv());
      result.match((error) => fail('Concat failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(1), IrInteger(2)])));
      });
    });

    test('concats non-empty list to empty list', () {
      final args = [
        IrList([]),
        IrList([IrInteger(3), IrInteger(4)]),
      ];
      final result = runEvalSimple(apply(concat, args), emptyEnv());
      result.match((error) => fail('Concat failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(3), IrInteger(4)])));
      });
    });

    test('concats two empty lists', () {
      final args = [IrList([]), IrList([])];
      final result = runEvalSimple(apply(concat, args), emptyEnv());
      result.match((error) => fail('Concat failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([])));
      });
    });

    test('fails on non-list first argument', () {
      final args = [
        IrInteger(42),
        IrList([IrInteger(1)]),
      ];
      final result = runEvalSimple(apply(concat, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list second argument', () {
      final args = [
        IrList([IrInteger(1)]),
        IrInteger(42),
      ];
      final result = runEvalSimple(apply(concat, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
