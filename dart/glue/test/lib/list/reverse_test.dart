import 'package:glue/src/eval.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/reverse.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Reverse (reverse)', () {
    test('reverses a list', () async {
      final args = [
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = await runEvalSimple(apply(reverse, args), emptyEnv());
      result.match((error) => fail('Reverse failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(3), IrInteger(2), IrInteger(1)])));
      });
    });

    test('reverses an empty list', () async {
      final args = [IrList([])];
      final result = await runEvalSimple(apply(reverse, args), emptyEnv());
      result.match((error) => fail('Reverse failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([])));
      });
    });

    test('reverses a list with multiple elements', () async {
      final args = [
        IrList([
          IrInteger(1),
          IrInteger(2),
          IrInteger(3),
          IrInteger(4),
          IrInteger(5),
        ]),
      ];
      final result = await runEvalSimple(apply(reverse, args), emptyEnv());
      result.match((error) => fail('Reverse failed: $error'), (value) {
        final (res, _) = value;
        expect(
          res,
          equals(
            IrList([
              IrInteger(5),
              IrInteger(4),
              IrInteger(3),
              IrInteger(2),
              IrInteger(1),
            ]),
          ),
        );
      });
    });

    test('fails on non-list', () async {
      final args = [IrInteger(42)];
      final result = await runEvalSimple(apply(reverse, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final args = <Ir>[];
      final result = await runEvalSimple(apply(reverse, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final args = [
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
        IrList([IrInteger(4), IrInteger(5), IrInteger(6)]),
      ];
      final result = await runEvalSimple(apply(reverse, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
