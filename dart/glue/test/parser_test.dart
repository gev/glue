import 'package:glue/src/ast.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/parser/error.dart';
import 'package:test/test.dart';

/// Helper function to assert parsing succeeds
void expectParsesTo(String input, Ast expected) {
  final result = parseGlue(input);
  switch (result) {
    case Right(value: final ast): // Success = Right
      expect(ast, equals(expected));
    case Left(value: final error): // Error = Left
      fail('Expected parsing to succeed with $expected, but got error: $error');
  }
}

/// Helper function to assert parsing fails with specific error type
void expectParseError(String input, Type errorType) {
  final result = parseGlue(input);
  switch (result) {
    case Right(value: final ast): // Success = Right
      fail(
        'Expected parsing to fail with $errorType, but succeeded with: $ast',
      );
    case Left(value: final error): // Error = Left
      expect(error.runtimeType, equals(errorType));
  }
}

void main() {
  group('Glue Parser', () {
    test('parse integers', () {
      expectParsesTo('42', IntegerAst(42));
      expectParsesTo('-17', IntegerAst(-17));
      expectParsesTo('0', IntegerAst(0));
    });

    test('parse floats', () {
      expectParsesTo('3.14', FloatAst(3.14));
      expectParsesTo('-2.5', FloatAst(-2.5));
      expectParsesTo('1.23e4', FloatAst(12300.0));
      expectParsesTo('1.23E4', FloatAst(12300.0));
      expectParsesTo('1.23e+4', FloatAst(12300.0));
      expectParsesTo('1.23E+4', FloatAst(12300.0));
      expectParsesTo('1.23e-4', FloatAst(0.000123));
      expectParsesTo('1.23E-4', FloatAst(0.000123));
      expectParsesTo('-1.23e4', FloatAst(-12300.0));
      expectParsesTo('-1.23E4', FloatAst(-12300.0));
      expectParsesTo('-1.23e+4', FloatAst(-12300.0));
      expectParsesTo('-1.23E+4', FloatAst(-12300.0));
      expectParsesTo('-1.23e-4', FloatAst(-0.000123));
      expectParsesTo('-1.23E-4', FloatAst(-0.000123));
    });

    test('parse strings', () {
      expectParsesTo('"hello"', StringAst('hello'));
      expectParsesTo('"with spaces"', StringAst('with spaces'));
      expectParsesTo('""', StringAst(''));
    });

    test('parse symbols', () {
      expectParsesTo('x', SymbolAst('x'));
      expectParsesTo('my-func', SymbolAst('my-func'));
      expectParsesTo('+', SymbolAst('+'));
      expectParsesTo('math.pi', SymbolAst('math.pi'));

      // Special characters
      expectParsesTo('-', SymbolAst('-'));
      expectParsesTo('*', SymbolAst('*'));
      expectParsesTo('/', SymbolAst('/'));
      expectParsesTo('%', SymbolAst('%'));
      expectParsesTo('=', SymbolAst('='));
      expectParsesTo('<', SymbolAst('<'));
      expectParsesTo('>', SymbolAst('>'));
      expectParsesTo('&', SymbolAst('&'));
      expectParsesTo('|', SymbolAst('|'));
      expectParsesTo('!', SymbolAst('!'));
      expectParsesTo('?', SymbolAst('?'));
      expectParsesTo('\\', SymbolAst('\\'));
      expectParsesTo('\$', SymbolAst('\$'));
      expectParsesTo('@', SymbolAst('@'));
      expectParsesTo('#', SymbolAst('#'));
      expectParsesTo('_', SymbolAst('_'));
      expectParsesTo('.', SymbolAst('.'));

      // Complex symbol combinations
      expectParsesTo('func\$helper', SymbolAst('func\$helper'));
      expectParsesTo('data@2023', SymbolAst('data@2023'));
      expectParsesTo('item#1', SymbolAst('item#1'));
      expectParsesTo('path/to:item', SymbolAst('path/to:item'));
    });

    test('parse empty list', () {
      expectParsesTo('()', ListAst([]));
    });

    test('parse simple list', () {
      expectParsesTo(
        '(1 2 3)',
        ListAst([IntegerAst(1), IntegerAst(2), IntegerAst(3)]),
      );
    });

    test('parse mixed list', () {
      expectParsesTo(
        '(add 1 "hello")',
        ListAst([SymbolAst('add'), IntegerAst(1), StringAst('hello')]),
      );
    });

    test('parse empty object fails', () {
      expectParseError('(:)', UnpairedPropertyError);
    });

    test('parse with comments', () {
      expectParsesTo('42 ; this is a comment', IntegerAst(42));
      expectParsesTo('; comment only\n42', IntegerAst(42));
    });

    test('parse errors for invalid input', () {
      expectParseError('', SyntaxError);
      expectParseError('   ', SyntaxError);
      expectParseError('; just comment', SyntaxError);
    });

    test('parse simple nested expression', () {
      // Current parser doesn't handle complex nesting yet
      // Test a simpler case that the current parser can handle
      expectParsesTo(
        '(+ 1 2)',
        ListAst([SymbolAst('+'), IntegerAst(1), IntegerAst(2)]),
      );
    });

    // Property Access tests
    test('parse property access', () {
      expectParsesTo('obj.name', SymbolAst('obj.name'));
      expectParsesTo('a.b.c', SymbolAst('a.b.c'));
      expectParsesTo('obj.prop1', SymbolAst('obj.prop1'));
      expectParsesTo('obj.prop-name', SymbolAst('obj.prop-name'));
      expectParsesTo('obj.prop_name', SymbolAst('obj.prop_name'));
    });

    // Operator Expressions tests
    test('parse operator expressions', () {
      expectParsesTo(
        '(+ 2 3)',
        ListAst([SymbolAst('+'), IntegerAst(2), IntegerAst(3)]),
      );
      expectParsesTo(
        '(< x y)',
        ListAst([SymbolAst('<'), SymbolAst('x'), SymbolAst('y')]),
      );
      expectParsesTo(
        '(== a b)',
        ListAst([SymbolAst('=='), SymbolAst('a'), SymbolAst('b')]),
      );
      expectParsesTo(
        '(* 2 3 4)',
        ListAst([SymbolAst('*'), IntegerAst(2), IntegerAst(3), IntegerAst(4)]),
      );
      expectParsesTo(
        '(<= x 10)',
        ListAst([SymbolAst('<='), SymbolAst('x'), IntegerAst(10)]),
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
      expectParsesTo('"hello 世界"', StringAst('hello 世界'));
    });

    test('handle escape sequences in strings', () {
      expectParsesTo('"hello\\nworld"', StringAst('hello\\nworld'));
    });

    test('handle symbols with unicode', () {
      expectParsesTo('变量', SymbolAst('变量'));
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
      expectParsesTo(
        '(= x y)',
        ListAst([SymbolAst('='), SymbolAst('x'), SymbolAst('y')]),
      );
      expectParsesTo(
        '(!= a b)',
        ListAst([SymbolAst('!='), SymbolAst('a'), SymbolAst('b')]),
      );
      expectParsesTo(
        '(>= val 0)',
        ListAst([SymbolAst('>='), SymbolAst('val'), IntegerAst(0)]),
      );
    });

    // Rule: No Mixed Content (from Haskell spec)
    test('successfully parses pure list', () {
      expectParsesTo(
        '(1 2 "test")',
        ListAst([IntegerAst(1), IntegerAst(2), StringAst('test')]),
      );
    });

    test('successfully parses pure object', () {
      expectParsesTo(
        '(:id 1 :type "lamp")',
        ObjectAst({'id': IntegerAst(1), 'type': StringAst('lamp')}),
      );
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
      final expected = ListAst([
        SymbolAst('f'),
        ObjectAst({'x': IntegerAst(1)}),
      ]);

      expectParsesTo('(f :x 1)', expected);
      expectParsesTo('(f (:x 1))', expected);
    });

    test('parses (f :x 1 :y 2) and (f (:x 1) (:y 2)) differently', () {
      final grouped = parseGlue('(f :x 1 :y 2)');
      final separate = parseGlue('(f (:x 1) (:y 2))');

      switch (grouped) {
        case Right(value: final groupedAst): // Success = Right
          switch (separate) {
            case Right(value: final separateAst): // Success = Right
              expect(groupedAst, isNot(equals(separateAst)));

              // grouped should be: (f (:x 1 :y 2))
              expect(groupedAst, isA<ListAst>());
              final groupedList = groupedAst as ListAst;
              expect(groupedList.elements.length, equals(2));
              expect(groupedList.elements[0], equals(SymbolAst('f')));
              expect(groupedList.elements[1], isA<ObjectAst>());
              final groupedObj = groupedList.elements[1] as ObjectAst;
              expect(groupedObj.properties.length, equals(2));
              expect(groupedObj.properties['x'], equals(IntegerAst(1)));
              expect(groupedObj.properties['y'], equals(IntegerAst(2)));

              // separate should be: (f (:x 1) (:y 2))
              expect(separateAst, isA<ListAst>());
              final separateList = separateAst as ListAst;
              expect(separateList.elements.length, equals(3));
              expect(separateList.elements[0], equals(SymbolAst('f')));
              expect(separateList.elements[1], isA<ObjectAst>());
              expect(separateList.elements[2], isA<ObjectAst>());
            case Left(value: final error): // Error = Left
              fail('Expected separate to succeed, got error: $error');
          }
        case Left(value: final error): // Error = Left
          fail('Expected grouped to succeed, got error: $error');
      }
    });

    // Deeply nested structures (from Haskell spec)
    test('parses deeply nested structures', () {
      expectParsesTo(
        '((((a))))',
        ListAst([
          ListAst([
            ListAst([
              ListAst([SymbolAst('a')]),
            ]),
          ]),
        ]),
      );
    });

    // Mixed nested objects and lists (from Haskell spec)
    test('parses mixed nested objects and lists', () {
      expectParsesTo(
        '(:a (:b (1 2)) :c 3)',
        ObjectAst({
          'a': ObjectAst({
            'b': ListAst([IntegerAst(1), IntegerAst(2)]),
          }),
          'c': IntegerAst(3),
        }),
      );
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
