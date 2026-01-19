import 'package:glue/src/eval.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/last.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Last (last)', () {
    test('returns the last element of a list', () async {
      final args = [
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = await runEvalSimple(apply(last, args), emptyEnv());
      result.match((error) => fail('Last failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrInteger(3)));
      });
    });

    test('returns the only element of a single-element list', () async {
      final args = [
        IrList([IrInteger(42)]),
      ];
      final result = await runEvalSimple(apply(last, args), emptyEnv());
      result.match((error) => fail('Last failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrInteger(42)));
      });
    });

    test('returns string element', () async {
      final args = [
        IrList([IrString('hello'), IrString('world')]),
      ];
      final result = await runEvalSimple(apply(last, args), emptyEnv());
      result.match((error) => fail('Last failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrString('world')));
      });
    });

    test('fails on empty list', () async {
      final args = [IrList([])];
      final result = await runEvalSimple(apply(last, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list argument', () async {
      final args = [IrInteger(42)];
      final result = await runEvalSimple(apply(last, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final args = <Ir>[];
      final result = await runEvalSimple(apply(last, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final args = [
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
        IrList([IrInteger(4), IrInteger(5), IrInteger(6)]),
      ];
      final result = await runEvalSimple(apply(last, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
