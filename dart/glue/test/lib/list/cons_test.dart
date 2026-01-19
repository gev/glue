import 'package:glue/src/eval.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/cons.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Cons (cons)', () {
    test('constructs a list by prepending an element', () async {
      final args = [
        IrInteger(1),
        IrList([IrInteger(2), IrInteger(3)]),
      ];
      final result = await runEvalSimple(apply(cons, args), emptyEnv());
      result.match((error) => fail('Cons failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(1), IrInteger(2), IrInteger(3)])));
      });
    });

    test('constructs a list with empty tail', () async {
      final args = [IrInteger(42), IrList([])];
      final result = await runEvalSimple(apply(cons, args), emptyEnv());
      result.match((error) => fail('Cons failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(42)])));
      });
    });

    test('fails on non-list tail', () async {
      final args = [IrInteger(1), IrInteger(2)];
      final result = await runEvalSimple(apply(cons, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final args = [IrInteger(1)];
      final result = await runEvalSimple(apply(cons, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final args = [
        IrInteger(1),
        IrList([IrInteger(2), IrInteger(3)]),
        IrInteger(4),
      ];
      final result = await runEvalSimple(apply(cons, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
