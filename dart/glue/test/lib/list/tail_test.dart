import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/tail.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Tail (tail)', () {
    test('returns the rest of a list', () {
      final args = [
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = runEvalSimple(apply(tail, args), emptyEnv());
      result.match((error) => fail('Tail failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(2), IrInteger(3)])));
      });
    });

    test('fails on empty list', () {
      final args = [IrList([])];
      final result = runEvalSimple(apply(tail, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list', () {
      final args = [IrInteger(42)];
      final result = runEvalSimple(apply(tail, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
