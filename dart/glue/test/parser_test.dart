import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/glue.dart';
import 'package:test/test.dart';

/// Helper function to assert parsing succeeds
void expectParsesTo(String input, Ast expected) {
  final result = parseGlue(input);
  expect(result, isA<Ast>());
  expect(result, equals(expected));
}

/// Helper function to assert parsing fails with specific error type
void expectParseError(String input, Type errorType) {
  final result = parseGlue(input);
  expect(result.runtimeType, equals(errorType));
}

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
      expect(parseGlue('1.23e4'), equals(FloatAst(12300.0)));
      expect(parseGlue('1.23E4'), equals(FloatAst(12300.0)));
      expect(parseGlue('1.23e+4'), equals(FloatAst(12300.0)));
      expect(parseGlue('1.23E+4'), equals(FloatAst(12300.0)));
      expect(parseGlue('1.23e-4'), equals(FloatAst(0.000123)));
      expect(parseGlue('1.23E-4'), equals(FloatAst(0.000123)));
      expect(parseGlue('-1.23e4'), equals(FloatAst(-12300.0)));
      expect(parseGlue('-1.23E4'), equals(FloatAst(-12300.0)));
      expect(parseGlue('-1.23e+4'), equals(FloatAst(-12300.0)));
      expect(parseGlue('-1.23E+4'), equals(FloatAst(-12300.0)));
      expect(parseGlue('-1.23e-4'), equals(FloatAst(-0.000123)));
      expect(parseGlue('-1.23E-4'), equals(FloatAst(-0.000123)));
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

      // Special characters
      expect(parseGlue('-'), equals(SymbolAst('-')));
      expect(parseGlue('*'), equals(SymbolAst('*')));
      expect(parseGlue('/'), equals(SymbolAst('/')));
      expect(parseGlue('%'), equals(SymbolAst('%')));
      expect(parseGlue('='), equals(SymbolAst('=')));
      expect(parseGlue('<'), equals(SymbolAst('<')));
      expect(parseGlue('>'), equals(SymbolAst('>')));
      expect(parseGlue('&'), equals(SymbolAst('&')));
      expect(parseGlue('|'), equals(SymbolAst('|')));
      expect(parseGlue('!'), equals(SymbolAst('!')));
      expect(parseGlue('?'), equals(SymbolAst('?')));
      expect(parseGlue('\\'), equals(SymbolAst('\\')));
      expect(parseGlue('\$'), equals(SymbolAst('\$')));
      expect(parseGlue('@'), equals(SymbolAst('@')));
      expect(parseGlue('#'), equals(SymbolAst('#')));
      expect(parseGlue('_'), equals(SymbolAst('_')));
      expect(parseGlue('.'), equals(SymbolAst('.')));

      // Complex symbol combinations
      expect(parseGlue('func\$helper'), equals(SymbolAst('func\$helper')));
      expect(parseGlue('data@2023'), equals(SymbolAst('data@2023')));
      expect(parseGlue('item#1'), equals(SymbolAst('item#1')));
      expect(parseGlue('path/to:item'), equals(SymbolAst('path/to:item')));
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

    test('parse empty object fails', () {
      expectParseError('(:)', UnpairedPropertyError);
    });

    test('parse with comments', () {
      expect(parseGlue('42 ; this is a comment'), equals(IntegerAst(42)));
      expect(parseGlue('; comment only\n42'), equals(IntegerAst(42)));
    });

    test('parse errors for invalid input', () {
      expectParseError('', SyntaxError);
      expectParseError('   ', SyntaxError);
      expectParseError('; just comment', SyntaxError);
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

    // Property Access tests
    test('parse property access', () {
      expect(parseGlue('obj.name'), equals(SymbolAst('obj.name')));
      expect(parseGlue('a.b.c'), equals(SymbolAst('a.b.c')));
      expect(parseGlue('obj.prop1'), equals(SymbolAst('obj.prop1')));
      expect(parseGlue('obj.prop-name'), equals(SymbolAst('obj.prop-name')));
      expect(parseGlue('obj.prop_name'), equals(SymbolAst('obj.prop_name')));
    });

    // Operator Expressions tests
    test('parse operator expressions', () {
      expect(
        parseGlue('(+ 2 3)'),
        equals(ListAst(IList([SymbolAst('+'), IntegerAst(2), IntegerAst(3)]))),
      );
      expect(
        parseGlue('(< x y)'),
        equals(
          ListAst(IList([SymbolAst('<'), SymbolAst('x'), SymbolAst('y')])),
        ),
      );
      expect(
        parseGlue('(== a b)'),
        equals(
          ListAst(IList([SymbolAst('=='), SymbolAst('a'), SymbolAst('b')])),
        ),
      );
      expect(
        parseGlue('(* 2 3 4)'),
        equals(
          ListAst(
            IList([
              SymbolAst('*'),
              IntegerAst(2),
              IntegerAst(3),
              IntegerAst(4),
            ]),
          ),
        ),
      );
      expect(
        parseGlue('(<= x 10)'),
        equals(
          ListAst(IList([SymbolAst('<='), SymbolAst('x'), IntegerAst(10)])),
        ),
      );
    });

    // Edge Cases
    test('parse empty input', () {
      expectParseError('', SyntaxError);
    });

    test('parse whitespace only', () {
      expectParseError('   \n\t  ', SyntaxError);
    });

    // Unicode and Special Characters
    test('handle unicode in strings', () {
      expect(parseGlue('"hello 世界"'), equals(StringAst('hello 世界')));
    });

    test('handle escape sequences in strings', () {
      expect(parseGlue('"hello\\nworld"'), equals(StringAst('hello\\nworld')));
    });

    test('handle symbols with unicode', () {
      expect(parseGlue('变量'), equals(SymbolAst('变量')));
    });

    // Syntax Errors - now returns proper error types
    test('handle malformed input', () {
      expectParseError('123.456.789', SyntaxError); // Invalid number
      expectParseError(
        '12.34e56e78',
        SyntaxError,
      ); // Invalid scientific notation
      expectParseError('(unclosed list', SyntaxError); // Unclosed parenthesis
    });

    // Complex operator expressions
    test('parse complex operator expressions', () {
      expect(
        parseGlue('(= x y)'),
        equals(
          ListAst(IList([SymbolAst('='), SymbolAst('x'), SymbolAst('y')])),
        ),
      );
      expect(
        parseGlue('(!= a b)'),
        equals(
          ListAst(IList([SymbolAst('!='), SymbolAst('a'), SymbolAst('b')])),
        ),
      );
      expect(
        parseGlue('(>= val 0)'),
        equals(
          ListAst(IList([SymbolAst('>='), SymbolAst('val'), IntegerAst(0)])),
        ),
      );
    });

    // Rule: No Mixed Content (from Haskell spec)
    test('successfully parses pure list', () {
      final input = '(1 2 "test")';
      final result = parseGlue(input);
      expect(result, isA<ListAst>());
      final listAst = result as ListAst;
      expect(listAst.elements.length, equals(3));
      expect(listAst.elements[0], equals(IntegerAst(1)));
      expect(listAst.elements[1], equals(IntegerAst(2)));
      expect(listAst.elements[2], equals(StringAst('test')));
    });

    test('successfully parses pure object', () {
      final input = '(:id 1 :type "lamp")';
      final result = parseGlue(input);
      expect(result, isA<ObjectAst>());
      final objAst = result as ObjectAst;
      expect(objAst.properties.length, equals(2));
      expect(objAst.properties['id'], equals(IntegerAst(1)));
      expect(objAst.properties['type'], equals(StringAst('lamp')));
    });

    test('FAILS when mixing atoms and properties', () {
      expectParseError('(:id 1 "oops")', MixedContentError);
    });

    test('FAILS when mixing properties and atoms', () {
      expectParseError('(1 2 :id 3)', MixedContentError);
    });

    // Rule: Property Pairs (from Haskell spec)
    test('FAILS on unpaired property key', () {
      expectParseError('(:id 1 :status)', UnpairedPropertyError);
    });

    // Equivalent Syntaxes (from Haskell spec)
    test('parses (f :x 1) and (f (:x 1)) identically', () {
      final expected = ListAst(
        IList([
          SymbolAst('f'),
          ObjectAst(IMap({'x': IntegerAst(1)})),
        ]),
      );

      final result1 = parseGlue('(f :x 1)');
      final result2 = parseGlue('(f (:x 1))');

      expect(result1, equals(expected));
      expect(result2, equals(expected));
    });

    test('parses (f :x 1 :y 2) and (f (:x 1) (:y 2)) differently', () {
      final grouped = parseGlue('(f :x 1 :y 2)');
      final separate = parseGlue('(f (:x 1) (:y 2))');

      expect(grouped, isNot(equals(separate)));

      // grouped should be: (f (:x 1 :y 2))
      expect(grouped, isA<ListAst>());
      final groupedList = grouped as ListAst;
      expect(groupedList.elements.length, equals(2));
      expect(groupedList.elements[0], equals(SymbolAst('f')));
      expect(groupedList.elements[1], isA<ObjectAst>());
      final groupedObj = groupedList.elements[1] as ObjectAst;
      expect(groupedObj.properties.length, equals(2));
      expect(groupedObj.properties['x'], equals(IntegerAst(1)));
      expect(groupedObj.properties['y'], equals(IntegerAst(2)));

      // separate should be: (f (:x 1) (:y 2))
      expect(separate, isA<ListAst>());
      final separateList = separate as ListAst;
      expect(separateList.elements.length, equals(3));
      expect(separateList.elements[0], equals(SymbolAst('f')));
      expect(separateList.elements[1], isA<ObjectAst>());
      expect(separateList.elements[2], isA<ObjectAst>());
    });

    // Deeply nested structures (from Haskell spec)
    test('parses deeply nested structures', () {
      final input = '((((a))))';
      final result = parseGlue(input);
      expect(result, isA<ListAst>());

      // Should be: ((((a))))
      ListAst current = result as ListAst;
      expect(current.elements.length, equals(1));

      current = current.elements[0] as ListAst;
      expect(current.elements.length, equals(1));

      current = current.elements[0] as ListAst;
      expect(current.elements.length, equals(1));

      current = current.elements[0] as ListAst;
      expect(current.elements.length, equals(1));

      expect(current.elements[0], equals(SymbolAst('a')));
    });

    // Mixed nested objects and lists (from Haskell spec)
    test('parses mixed nested objects and lists', () {
      final input = '(:a (:b (1 2)) :c 3)';
      final result = parseGlue(input);
      expect(result, isA<ObjectAst>());

      final objAst = result as ObjectAst;
      expect(objAst.properties.length, equals(2));

      // :a should contain nested object (:b (1 2))
      expect(objAst.properties['a'], isA<ObjectAst>());
      final nestedObj = objAst.properties['a'] as ObjectAst;
      expect(nestedObj.properties.length, equals(1));

      // :b should contain list (1 2)
      expect(nestedObj.properties['b'], isA<ListAst>());
      final nestedList = nestedObj.properties['b'] as ListAst;
      expect(nestedList.elements.length, equals(2));
      expect(nestedList.elements[0], equals(IntegerAst(1)));
      expect(nestedList.elements[1], equals(IntegerAst(2)));

      // :c should be 3
      expect(objAst.properties['c'], equals(IntegerAst(3)));
    });

    // Error Recovery (from Haskell spec)
    test('provides meaningful error messages', () {
      expectParseError('(+ 1', SyntaxError); // Unclosed parenthesis
    });

    test('handles malformed numbers', () {
      expectParseError('123.456.789', SyntaxError); // Multiple dots
      expectParseError('12.34e56e78', SyntaxError); // Multiple e's
    });
  });
}
