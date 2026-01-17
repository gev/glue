import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:test/test.dart';

void main() {
  group('NativeFunc constructors and equality', () {
    test('creates NativeFunc', () {
      final nf = IrNativeFunc((List<Ir> args) => Eval.pure(IrInteger(42)));
      expect(nf, isNotNull);
    });

    test('NativeFunc are equal regardless of implementation', () {
      final f1 = IrNativeFunc((List<Ir> args) => Eval.pure(IrInteger(1)));
      final f2 = IrNativeFunc((List<Ir> args) => Eval.pure(IrInteger(2)));
      expect(f1 == f2, isTrue);
    });
  });
}
