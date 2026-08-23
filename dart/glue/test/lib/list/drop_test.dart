import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/drop.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Drop (drop)', () {
    test('drops first N elements from list', () {
      final args = [
        IrInteger(2),
        IrList([
          IrInteger(1),
          IrInteger(2),
          IrInteger(3),
          IrInteger(4),
          IrInteger(5),
        ]),
      ];
      final result = runEvalSimple(apply(drop, args), emptyEnv());
      result.match((error) => fail('Drop failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(3), IrInteger(4), IrInteger(5)])));
      });
    });

    test('drops fewer elements when N > list length', () {
      final args = [
        IrInteger(10),
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = runEvalSimple(apply(drop, args), emptyEnv());
      result.match((error) => fail('Drop failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([])));
      });
    });

    test('drops zero elements', () {
      final args = [
        IrInteger(0),
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = runEvalSimple(apply(drop, args), emptyEnv());
      result.match((error) => fail('Drop failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(1), IrInteger(2), IrInteger(3)])));
      });
    });

    test('drops all elements when N equals list length', () {
      final args = [
        IrInteger(3),
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = runEvalSimple(apply(drop, args), emptyEnv());
      result.match((error) => fail('Drop failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([])));
      });
    });

    test('fails on negative count', () {
      final args = [
        IrInteger(-1),
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = runEvalSimple(apply(drop, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails on non-number first argument', () {
      final args = [
        IrString('2'),
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = runEvalSimple(apply(drop, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list second argument', () {
      final args = [IrInteger(2), IrInteger(42)];
      final result = runEvalSimple(apply(drop, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
