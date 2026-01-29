import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/butlast.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Butlast (butlast)', () {
    test('returns all elements except the last one', () async {
      final args = [
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = await runEvalSimple(apply(butlast, args), emptyEnv());
      result.match((error) => fail('Butlast failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(1), IrInteger(2)])));
      });
    });

    test('returns empty list for single-element list', () async {
      final args = [
        IrList([IrInteger(42)]),
      ];
      final result = await runEvalSimple(apply(butlast, args), emptyEnv());
      result.match((error) => fail('Butlast failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([])));
      });
    });

    test('returns string elements except last', () async {
      final args = [
        IrList([IrString('hello'), IrString('world'), IrString('test')]),
      ];
      final result = await runEvalSimple(apply(butlast, args), emptyEnv());
      result.match((error) => fail('Butlast failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrString('hello'), IrString('world')])));
      });
    });

    test('fails on empty list', () async {
      final args = [IrList([])];
      final result = await runEvalSimple(apply(butlast, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list argument', () async {
      final args = [IrInteger(42)];
      final result = await runEvalSimple(apply(butlast, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
