import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/append.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Append (append)', () {
    test('appends two lists', () async {
      final args = [
        IrList([IrInteger(1), IrInteger(2)]),
        IrList([IrInteger(3), IrInteger(4)]),
      ];
      final result = await runEvalSimple(apply(append, args), emptyEnv());
      result.match((error) => fail('Append failed: $error'), (value) {
        final (res, _) = value;
        expect(
          res,
          equals(
            IrList([IrInteger(1), IrInteger(2), IrInteger(3), IrInteger(4)]),
          ),
        );
      });
    });

    test('appends empty list to non-empty list', () async {
      final args = [
        IrList([IrInteger(1), IrInteger(2)]),
        IrList([]),
      ];
      final result = await runEvalSimple(apply(append, args), emptyEnv());
      result.match((error) => fail('Append failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(1), IrInteger(2)])));
      });
    });

    test('appends non-empty list to empty list', () async {
      final args = [
        IrList([]),
        IrList([IrInteger(3), IrInteger(4)]),
      ];
      final result = await runEvalSimple(apply(append, args), emptyEnv());
      result.match((error) => fail('Append failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(3), IrInteger(4)])));
      });
    });

    test('appends two empty lists', () async {
      final args = [IrList([]), IrList([])];
      final result = await runEvalSimple(apply(append, args), emptyEnv());
      result.match((error) => fail('Append failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([])));
      });
    });

    test('fails on non-list first argument', () async {
      final args = [
        IrInteger(42),
        IrList([IrInteger(1)]),
      ];
      final result = await runEvalSimple(apply(append, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list second argument', () async {
      final args = [
        IrList([IrInteger(1)]),
        IrInteger(42),
      ];
      final result = await runEvalSimple(apply(append, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
