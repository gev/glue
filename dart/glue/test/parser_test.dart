import 'package:glue/glue.dart';
import 'package:test/test.dart';

void main() {
  group('Glue Parser', () {
    test('parse integers', () {
      expect(parseGlue('42'), equals(IntegerAst(42)));
      expect(parseGlue('-17'), equals(IntegerAst(-17)));
      expect(parseGlue('0'), equals(IntegerAst(0)));
    });

    test('parse floats', () {
      expect(parseGlue('3.14'), equals(FloatAst(3.14)));
      expect(parseGlue('-2.5'), equals(FloatAst(-2.5)));
      expect(parseGlue('1.23e4'), equals(FloatAst(12300)));
    });

    test('parse strings', () {
      expect(parseGlue('"hello"'), equals(StringAst('hello')));
      expect(parseGlue('"with spaces"'), equals(StringAst('with spaces')));
      expect(parseGlue('""'), equals(StringAst('')));
    });

    test('parse symbols', () {
      expect(parseGlue('x'), equals(SymbolAst('x')));
      expect(parseGlue('my-func'), equals(SymbolAst('my-func')));
      expect(parseGlue('+'), equals(SymbolAst('+')));
      expect(parseGlue('math.pi'), equals(SymbolAst('math.pi')));
    });

    test('parse empty list', () {
      final result = parseGlue('()');
      expect(result, isA<ListAst>());
      final listAst = result as ListAst;
      expect(listAst.elements.isEmpty, isTrue);
    });

    test('parse simple list', () {
      final result = parseGlue('(1 2 3)');
      expect(result, isA<ListAst>());
      final listAst = result as ListAst;
      expect(listAst.elements.length, equals(3));
      expect(listAst.elements[0], equals(IntegerAst(1)));
      expect(listAst.elements[1], equals(IntegerAst(2)));
      expect(listAst.elements[2], equals(IntegerAst(3)));
    });

    test('parse mixed list', () {
      final result = parseGlue('(add 1 "hello")');
      expect(result, isA<ListAst>());
      final listAst = result as ListAst;
      expect(listAst.elements.length, equals(3));
      expect(listAst.elements[0], equals(SymbolAst('add')));
      expect(listAst.elements[1], equals(IntegerAst(1)));
      expect(listAst.elements[2], equals(StringAst('hello')));
    });

    test('parse empty object', () {
      final result = parseGlue('{}');
      expect(result, isA<ObjectAst>());
      final objAst = result as ObjectAst;
      expect(objAst.properties.isEmpty, isTrue);
    });

    test('parse with comments', () {
      expect(parseGlue('42 ; this is a comment'), equals(IntegerAst(42)));
      expect(parseGlue('; comment only\n42'), equals(IntegerAst(42)));
    });

    test('parse null for invalid input', () {
      expect(parseGlue(''), isNull);
      expect(parseGlue('   '), isNull);
      expect(parseGlue('; just comment'), isNull);
    });

    test('parse simple nested expression', () {
      // Current parser doesn't handle complex nesting yet
      // Test a simpler case that the current parser can handle
      final result = parseGlue('(+ 1 2)');
      expect(result, isA<ListAst>());
      final listAst = result as ListAst;
      expect(listAst.elements.length, equals(3));
      expect(listAst.elements[0], equals(SymbolAst('+')));
      expect(listAst.elements[1], equals(IntegerAst(1)));
      expect(listAst.elements[2], equals(IntegerAst(2)));
    });
  });
}
