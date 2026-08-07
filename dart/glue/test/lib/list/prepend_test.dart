import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/prepend.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Prepend (prepend)', () {
    test('prependtructs a list by prepending an element', () async {
      final args = [
        IrInteger(1),
        IrList([IrInteger(2), IrInteger(3)]),
      ];
      final result = runEvalSimple(apply(prepend, args), emptyEnv());
      result.match((error) => fail('Prepend failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(1), IrInteger(2), IrInteger(3)])));
      });
    });

    test('prependtructs a list with empty tail', () async {
      final args = [IrInteger(42), IrList([])];
      final result = runEvalSimple(apply(prepend, args), emptyEnv());
      result.match((error) => fail('Prepend failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrList([IrInteger(42)])));
      });
    });

    test('fails on non-list tail', () async {
      final args = [IrInteger(1), IrInteger(2)];
      final result = runEvalSimple(apply(prepend, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
