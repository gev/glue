import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/append.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Append (append)', () {
    test('appendtructs a list by appending an element', () {
      final args = [
        IrList([IrInteger(1), IrInteger(2)]),
        IrInteger(3),
      ];
      final result = runEvalSimple(apply(append, args), emptyEnv());
      result.match((error) => fail('Append failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(1), IrInteger(2), IrInteger(3)])));
      });
    });

    test('appendtructs a list with empty tail', () {
      final args = [IrList([]), IrInteger(42)];
      final result = runEvalSimple(apply(append, args), emptyEnv());
      result.match((error) => fail('Append failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(42)])));
      });
    });

    test('fails on non-list head', () {
      final args = [IrInteger(1), IrInteger(2)];
      final result = runEvalSimple(apply(append, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
