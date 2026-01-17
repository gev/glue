import 'package:glue/src/ir.dart';
import 'package:test/test.dart';

void main() {
  group('Special form constructors and equality', () {
    test('creates Special', () {
      final special = IrSpecial((List<Ir> args) => IrVoid());
      expect(special, isNotNull);
    });

    test('Special forms are equal regardless of implementation', () {
      final s1 = IrSpecial((List<Ir> args) => IrInteger(1));
      final s2 = IrSpecial((List<Ir> args) => IrInteger(2));
      expect(s1 == s2, isTrue);
    });
  });
}
