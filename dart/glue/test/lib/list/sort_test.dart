import 'package:glue/src/eval.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/sort.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Sort (sort)', () {
    test('sorts a list of numbers in ascending order', () async {
      final args = [
        IrList([
          IrInteger(3),
          IrInteger(1),
          IrInteger(4),
          IrInteger(1),
          IrInteger(5),
        ]),
      ];
      final result = await runEvalSimple(apply(sort, args), emptyEnv());
      result.match((error) => fail('Sort failed: $error'), (value) {
        final (res, _) = value;
        expect(
          res,
          equals(
            IrList([
              IrInteger(1),
              IrInteger(1),
              IrInteger(3),
              IrInteger(4),
              IrInteger(5),
            ]),
          ),
        );
      });
    });

    test('sorts a list of strings in alphabetical order', () async {
      final args = [
        IrList([IrString('zebra'), IrString('apple'), IrString('banana')]),
      ];
      final result = await runEvalSimple(apply(sort, args), emptyEnv());
      result.match((error) => fail('Sort failed: $error'), (value) {
        final (res, _) = value;
        expect(
          res,
          equals(
            IrList([IrString('apple'), IrString('banana'), IrString('zebra')]),
          ),
        );
      });
    });

    test('sorts an empty list', () async {
      final args = [IrList([])];
      final result = await runEvalSimple(apply(sort, args), emptyEnv());
      result.match((error) => fail('Sort failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([])));
      });
    });

    test('sorts a single element list', () async {
      final args = [
        IrList([IrInteger(42)]),
      ];
      final result = await runEvalSimple(apply(sort, args), emptyEnv());
      result.match((error) => fail('Sort failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(42)])));
      });
    });

    test('sorts a list with duplicate elements', () async {
      final args = [
        IrList([
          IrInteger(3),
          IrInteger(1),
          IrInteger(3),
          IrInteger(1),
          IrInteger(2),
        ]),
      ];
      final result = await runEvalSimple(apply(sort, args), emptyEnv());
      result.match((error) => fail('Sort failed: $error'), (value) {
        final (res, _) = value;
        expect(
          res,
          equals(
            IrList([
              IrInteger(1),
              IrInteger(1),
              IrInteger(2),
              IrInteger(3),
              IrInteger(3),
            ]),
          ),
        );
      });
    });

    test('fails on non-list argument', () async {
      final args = [IrInteger(42)];
      final result = await runEvalSimple(apply(sort, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails on list with incomparable elements', () async {
      final args = [
        IrList([
          IrInteger(1),
          IrList([IrInteger(2)]),
        ]),
      ];
      final result = await runEvalSimple(apply(sort, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
