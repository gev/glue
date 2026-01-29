import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list/car.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.List.Car (car)', () {
    test('returns the first element of a list', () async {
      final args = [
        IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
      ];
      final result = await runEvalSimple(apply(car, args), emptyEnv());
      result.match((error) => fail('Car failed: $error'), (value) {
        final (res, _) = value;
        expect(res, equals(IrInteger(1)));
      });
    });

    test('fails on empty list', () async {
      final args = [IrList([])];
      final result = await runEvalSimple(apply(car, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list', () async {
      final args = [IrInteger(42)];
      final result = await runEvalSimple(apply(car, args), emptyEnv());
      expect(result.isLeft, isTrue);
    });
  });
}
