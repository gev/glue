import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/take.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Take (take)', () {
    test('takes first N elements from list', () async {
      final args = [
        IrInteger(3),
        IrList([
          IrInteger(1),
          IrInteger(2),
          IrInteger(3),
          IrInteger(4),
          IrInteger(5),
        ]),
      ];
      final result = await runEvalSimple(apply(take, args), emptyEnv());
      result.match((error) => fail('Take failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(1), IrInteger(2), IrInteger(3)])));
      });
    });

    test('takes fewer elements when N > list length', () async {
      final args = [
        IrInteger(10),
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = await runEvalSimple(apply(take, args), emptyEnv());
      result.match((error) => fail('Take failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(1), IrInteger(2), IrInteger(3)])));
      });
    });

    test('takes zero elements', () async {
      final args = [
        IrInteger(0),
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = await runEvalSimple(apply(take, args), emptyEnv());
      result.match((error) => fail('Take failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([])));
      });
    });

    test('takes all elements when N equals list length', () async {
      final args = [
        IrInteger(3),
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = await runEvalSimple(apply(take, args), emptyEnv());
      result.match((error) => fail('Take failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(1), IrInteger(2), IrInteger(3)])));
      });
    });

    test('fails on negative count', () async {
      final args = [
        IrInteger(-1),
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = await runEvalSimple(apply(take, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails on non-number first argument', () async {
      final args = [
        IrString('3'),
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = await runEvalSimple(apply(take, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list second argument', () async {
      final args = [IrInteger(3), IrInteger(42)];
      final result = await runEvalSimple(apply(take, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
