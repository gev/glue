import 'package:glue/glue.dart' as glue;
import 'package:test/test.dart';

void main() {
  group('IR', () {
    test('IrInteger equality and toString', () {
      final ir1 = glue.IrInteger(42);
      final ir2 = glue.IrInteger(42);
      final ir3 = glue.IrInteger(24);

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('42'));
    });

    test('IrFloat equality and toString', () {
      final ir1 = glue.IrFloat(3.14);
      final ir2 = glue.IrFloat(3.14);
      final ir3 = glue.IrFloat(2.71);

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('3.14'));
    });

    test('IrString equality and toString', () {
      final ir1 = glue.IrString('hello');
      final ir2 = glue.IrString('hello');
      final ir3 = glue.IrString('world');

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('"hello"'));
    });

    test('IrBool equality and toString', () {
      final ir1 = glue.IrBool(true);
      final ir2 = glue.IrBool(true);
      final ir3 = glue.IrBool(false);

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('true'));
      expect(ir3.toString(), equals('false'));
    });

    test('IrSymbol equality and toString', () {
      final ir1 = glue.IrSymbol('x');
      final ir2 = glue.IrSymbol('x');
      final ir3 = glue.IrSymbol('y');

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('x'));
    });

    test('IrDottedSymbol equality and toString', () {
      final ir1 = glue.IrDottedSymbol(['a', 'b', 'c']);
      final ir2 = glue.IrDottedSymbol(['a', 'b', 'c']);
      final ir3 = glue.IrDottedSymbol(['x', 'y']);

      expect(ir1 == ir2, isTrue);
      expect(ir1 == ir3, isFalse);
      expect(ir1.toString(), equals('a.b.c'));
    });

    test('IrList equality and toString', () {
      final ir1 = glue.IrList([glue.IrInteger(1), glue.IrString('hello')]);
      final ir2 = glue.IrList([glue.IrInteger(1), glue.IrString('hello')]);
      final ir3 = glue.IrList([glue.IrInteger(1), glue.IrString('world')]);

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('(1 "hello")'));
    });

    test('IrObject equality and toString', () {
      final ir1 = glue.IrObject({
        'name': glue.IrString('Alice'),
        'age': glue.IrInteger(30),
      });
      final ir2 = glue.IrObject({
        'name': glue.IrString('Alice'),
        'age': glue.IrInteger(30),
      });
      final ir3 = glue.IrObject({
        'name': glue.IrString('Bob'),
        'age': glue.IrInteger(25),
      });

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('{object}'));
    });

    test('IrVoid equality and toString', () {
      final ir1 = glue.IrVoid();
      final ir2 = glue.IrVoid();

      expect(ir1, equals(ir2));
      expect(ir1.toString(), equals('#<void>'));
    });
  });

  group('AST -> IR compilation', () {
    test('compiles StringAst to IrString', () {
      final ast = glue.StringAst('hello');
      final ir = glue.compile(ast);
      expect(ir, isA<glue.IrString>());
      expect((ir as glue.IrString).value, equals('hello'));
    });

    test('compiles IntegerAst to IrInteger', () {
      final ast = glue.IntegerAst(42);
      final ir = glue.compile(ast);
      expect(ir, isA<glue.IrInteger>());
      expect((ir as glue.IrInteger).value, equals(42));
    });

    test('compiles FloatAst to IrFloat', () {
      final ast = glue.FloatAst(3.14);
      final ir = glue.compile(ast);
      expect(ir, isA<glue.IrFloat>());
      expect((ir as glue.IrFloat).value, closeTo(3.14, 0.001));
    });

    test('compiles SymbolAst without dots to IrSymbol', () {
      final ast = glue.SymbolAst('x');
      final ir = glue.compile(ast);
      expect(ir, isA<glue.IrSymbol>());
      expect((ir as glue.IrSymbol).value, equals('x'));
    });

    test('compiles SymbolAst with dots to IrDottedSymbol', () {
      final ast = glue.SymbolAst('a.b.c');
      final ir = glue.compile(ast);
      expect(ir, isA<glue.IrDottedSymbol>());
      expect((ir as glue.IrDottedSymbol).parts, equals(['a', 'b', 'c']));
    });

    test('compiles ListAst to IrList preserving length', () {
      final ast = glue.ListAst([
        glue.IntegerAst(1),
        glue.StringAst('hello'),
        glue.SymbolAst('x'),
      ]);
      final ir = glue.compile(ast);
      expect(ir, isA<glue.IrList>());
      final irList = ir as glue.IrList;
      expect(irList.elements.length, equals(3));
      expect(irList.elements[0], isA<glue.IrInteger>());
      expect(irList.elements[1], isA<glue.IrString>());
      expect(irList.elements[2], isA<glue.IrSymbol>());
    });

    test('compiles ObjectAst to IrObject', () {
      final ast = glue.ObjectAst({
        'name': glue.StringAst('Alice'),
        'age': glue.IntegerAst(30),
        'nested': glue.ObjectAst({'key': glue.SymbolAst('value')}),
      });
      final ir = glue.compile(ast);
      expect(ir, isA<glue.IrObject>());
      final irObj = ir as glue.IrObject;
      expect(irObj.properties.length, equals(3));
      expect(irObj.properties['name'], isA<glue.IrString>());
      expect(irObj.properties['age'], isA<glue.IrInteger>());
      expect(irObj.properties['nested'], isA<glue.IrObject>());
    });

    test('compiles empty ListAst', () {
      final ast = glue.ListAst([]);
      final ir = glue.compile(ast);
      expect(ir, isA<glue.IrList>());
      expect((ir as glue.IrList).elements.length, equals(0));
    });

    test('compiles empty ObjectAst', () {
      final ast = glue.ObjectAst({});
      final ir = glue.compile(ast);
      expect(ir, isA<glue.IrObject>());
      expect((ir as glue.IrObject).properties.length, equals(0));
    });

    test('recursive compilation in nested structures', () {
      final ast = glue.ListAst([
        glue.ObjectAst({'a': glue.IntegerAst(1)}),
        glue.ListAst([glue.SymbolAst('x.y'), glue.FloatAst(2.5)]),
      ]);
      final ir = glue.compile(ast);
      expect(ir, isA<glue.IrList>());
      final irList = ir as glue.IrList;
      expect(irList.elements[0], isA<glue.IrObject>());
      expect(irList.elements[1], isA<glue.IrList>());

      final nestedObj = irList.elements[0] as glue.IrObject;
      expect(nestedObj.properties['a'], isA<glue.IrInteger>());

      final nestedList = irList.elements[1] as glue.IrList;
      expect(nestedList.elements[0], isA<glue.IrDottedSymbol>());
      expect(nestedList.elements[1], isA<glue.IrFloat>());
    });
  });

  group('IR helper functions', () {
    test('isList and listLength', () {
      final listIr = glue.IrList([glue.IrInteger(1), glue.IrInteger(2)]);
      final nonListIr = glue.IrInteger(42);

      expect(glue.isList(listIr), isTrue);
      expect(glue.isList(nonListIr), isFalse);
      expect(glue.listLength(listIr), equals(2));
      expect(glue.listLength(nonListIr), equals(0));
    });

    test('isObject and objectSize', () {
      final objIr = glue.IrObject({
        'a': glue.IrInteger(1),
        'b': glue.IrString('hello'),
      });
      final nonObjIr = glue.IrSymbol('x');

      expect(glue.isObject(objIr), isTrue);
      expect(glue.isObject(nonObjIr), isFalse);
      expect(glue.objectSize(objIr), equals(2));
      expect(glue.objectSize(nonObjIr), equals(0));
    });

    test('objectLookup', () {
      final objIr = glue.IrObject({
        'key': glue.IrString('value'),
        'num': glue.IrInteger(42),
      });
      final nonObjIr = glue.IrList([]);

      expect(glue.objectLookup('key', objIr), isA<glue.IrString>());
      expect(
        (glue.objectLookup('key', objIr) as glue.IrString).value,
        equals('value'),
      );
      expect(glue.objectLookup('missing', objIr), isNull);
      expect(glue.objectLookup('key', nonObjIr), isNull);
    });

    test('isSymbol and getSymbol', () {
      final symbolIr = glue.IrSymbol('x');
      final dottedIr = glue.IrDottedSymbol(['a', 'b']);
      final nonSymbolIr = glue.IrInteger(42);

      expect(glue.isSymbol(symbolIr), isTrue);
      expect(glue.isSymbol(dottedIr), isTrue);
      expect(glue.isSymbol(nonSymbolIr), isFalse);

      expect(glue.getSymbol(symbolIr), equals('x'));
      expect(glue.getSymbol(dottedIr), equals('a.b'));
      expect(glue.getSymbol(nonSymbolIr), equals(''));
    });
  });

  group('Compilation properties', () {
    test('list length preservation', () {
      final asts = [
        glue.IntegerAst(1),
        glue.StringAst('hello'),
        glue.SymbolAst('x'),
        glue.FloatAst(3.14),
      ];
      final ast = glue.ListAst(asts);
      final ir = glue.compile(ast);

      expect(glue.isList(ir), isTrue);
      expect(glue.listLength(ir), equals(asts.length));
    });

    test('object size properties', () {
      final props = {
        'a': glue.IntegerAst(1),
        'b': glue.StringAst('hello'),
        'c': glue.SymbolAst('x'),
      };
      final ast = glue.ObjectAst(props);
      final ir = glue.compile(ast);

      expect(glue.isObject(ir), isTrue);
      expect(glue.objectSize(ir), equals(props.length));
    });

    test('symbol compilation idempotent', () {
      final symbols = ['x', 'hello', 'a.b.c', 'module.function'];
      for (final sym in symbols) {
        final ast = glue.SymbolAst(sym);
        final ir = glue.compile(ast);

        expect(glue.isSymbol(ir), isTrue);
        expect(glue.getSymbol(ir), equals(sym));
      }
    });

    test('recursive object lookup', () {
      final innerAst = glue.IntegerAst(42);
      final ast = glue.ObjectAst({
        'outer': glue.ObjectAst({'inner': innerAst}),
      });
      final ir = glue.compile(ast);

      expect(glue.isObject(ir), isTrue);
      final outerObj = ir as glue.IrObject;
      final innerIr = outerObj.properties['outer'];
      expect(innerIr, isA<glue.IrObject>());

      final innerObj = innerIr as glue.IrObject;
      final valueIr = innerObj.properties['inner'];
      expect(valueIr, isA<glue.IrInteger>());
      expect((valueIr as glue.IrInteger).value, equals(42));
    });
  });
}
