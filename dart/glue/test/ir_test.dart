import 'package:glue/src/ast.dart';
import 'package:glue/src/ir.dart';
import 'package:test/test.dart' hide isList;

void main() {
  group('IR', () {
    test('IrInteger equality and toString', () {
      final ir1 = IrInteger(42);
      final ir2 = IrInteger(42);
      final ir3 = IrInteger(24);

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('42'));
    });

    test('IrFloat equality and toString', () {
      final ir1 = IrFloat(3.14);
      final ir2 = IrFloat(3.14);
      final ir3 = IrFloat(2.71);

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('3.14'));
    });

    test('IrString equality and toString', () {
      final ir1 = IrString('hello');
      final ir2 = IrString('hello');
      final ir3 = IrString('world');

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('"hello"'));
    });

    test('IrBool equality and toString', () {
      final ir1 = IrBool(true);
      final ir2 = IrBool(true);
      final ir3 = IrBool(false);

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('true'));
      expect(ir3.toString(), equals('false'));
    });

    test('IrSymbol equality and toString', () {
      final ir1 = IrSymbol('x');
      final ir2 = IrSymbol('x');
      final ir3 = IrSymbol('y');

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('x'));
    });

    test('IrDottedSymbol equality and toString', () {
      final ir1 = IrDottedSymbol(['a', 'b', 'c']);
      final ir2 = IrDottedSymbol(['a', 'b', 'c']);
      final ir3 = IrDottedSymbol(['x', 'y']);

      expect(ir1 == ir2, isTrue);
      expect(ir1 == ir3, isFalse);
      expect(ir1.toString(), equals('a.b.c'));
    });

    test('IrList equality and toString', () {
      final ir1 = IrList([IrInteger(1), IrString('hello')]);
      final ir2 = IrList([IrInteger(1), IrString('hello')]);
      final ir3 = IrList([IrInteger(1), IrString('world')]);

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('(1 "hello")'));
    });

    test('IrObject equality and toString', () {
      final ir1 = IrObject({'name': IrString('Alice'), 'age': IrInteger(30)});
      final ir2 = IrObject({'name': IrString('Alice'), 'age': IrInteger(30)});
      final ir3 = IrObject({'name': IrString('Bob'), 'age': IrInteger(25)});

      expect(ir1, equals(ir2));
      expect(ir1, isNot(equals(ir3)));
      expect(ir1.toString(), equals('{object}'));
    });

    test('IrVoid equality and toString', () {
      final ir1 = IrVoid();
      final ir2 = IrVoid();

      expect(ir1, equals(ir2));
      expect(ir1.toString(), equals('#<void>'));
    });
  });

  group('AST -> IR compilation', () {
    test('compiles StringAst to IrString', () {
      final ast = StringAst('hello');
      final ir = compile(ast);
      expect(ir, isA<IrString>());
      expect((ir as IrString).value, equals('hello'));
    });

    test('compiles IntegerAst to IrInteger', () {
      final ast = IntegerAst(42);
      final ir = compile(ast);
      expect(ir, isA<IrInteger>());
      expect((ir as IrInteger).value, equals(42));
    });

    test('compiles FloatAst to IrFloat', () {
      final ast = FloatAst(3.14);
      final ir = compile(ast);
      expect(ir, isA<IrFloat>());
      expect((ir as IrFloat).value, closeTo(3.14, 0.001));
    });

    test('compiles SymbolAst without dots to IrSymbol', () {
      final ast = SymbolAst('x');
      final ir = compile(ast);
      expect(ir, isA<IrSymbol>());
      expect((ir as IrSymbol).value, equals('x'));
    });

    test('compiles SymbolAst with dots to IrDottedSymbol', () {
      final ast = SymbolAst('a.b.c');
      final ir = compile(ast);
      expect(ir, isA<IrDottedSymbol>());
      expect((ir as IrDottedSymbol).parts, equals(['a', 'b', 'c']));
    });

    test('compiles ListAst to IrList preserving length', () {
      final ast = ListAst([IntegerAst(1), StringAst('hello'), SymbolAst('x')]);
      final ir = compile(ast);
      expect(ir, isA<IrList>());
      final irList = ir as IrList;
      expect(irList.elements.length, equals(3));
      expect(irList.elements[0], isA<IrInteger>());
      expect(irList.elements[1], isA<IrString>());
      expect(irList.elements[2], isA<IrSymbol>());
    });

    test('compiles ObjectAst to IrObject', () {
      final ast = ObjectAst({
        'name': StringAst('Alice'),
        'age': IntegerAst(30),
        'nested': ObjectAst({'key': SymbolAst('value')}),
      });
      final ir = compile(ast);
      expect(ir, isA<IrObject>());
      final irObj = ir as IrObject;
      expect(irObj.properties.length, equals(3));
      expect(irObj.properties['name'], isA<IrString>());
      expect(irObj.properties['age'], isA<IrInteger>());
      expect(irObj.properties['nested'], isA<IrObject>());
    });

    test('compiles empty ListAst', () {
      final ast = ListAst([]);
      final ir = compile(ast);
      expect(ir, isA<IrList>());
      expect((ir as IrList).elements.length, equals(0));
    });

    test('compiles empty ObjectAst', () {
      final ast = ObjectAst({});
      final ir = compile(ast);
      expect(ir, isA<IrObject>());
      expect((ir as IrObject).properties.length, equals(0));
    });

    test('recursive compilation in nested structures', () {
      final ast = ListAst([
        ObjectAst({'a': IntegerAst(1)}),
        ListAst([SymbolAst('x.y'), FloatAst(2.5)]),
      ]);
      final ir = compile(ast);
      expect(ir, isA<IrList>());
      final irList = ir as IrList;
      expect(irList.elements[0], isA<IrObject>());
      expect(irList.elements[1], isA<IrList>());

      final nestedObj = irList.elements[0] as IrObject;
      expect(nestedObj.properties['a'], isA<IrInteger>());

      final nestedList = irList.elements[1] as IrList;
      expect(nestedList.elements[0], isA<IrDottedSymbol>());
      expect(nestedList.elements[1], isA<IrFloat>());
    });
  });

  group('IR helper functions', () {
    test('isList and listLength', () {
      final listIr = IrList([IrInteger(1), IrInteger(2)]);
      final nonListIr = IrInteger(42);

      expect(isList(listIr), isTrue);
      expect(isList(nonListIr), isFalse);
      expect(listLength(listIr), equals(2));
      expect(listLength(nonListIr), equals(0));
    });

    test('isObject and objectSize', () {
      final objIr = IrObject({'a': IrInteger(1), 'b': IrString('hello')});
      final nonObjIr = IrSymbol('x');

      expect(isObject(objIr), isTrue);
      expect(isObject(nonObjIr), isFalse);
      expect(objectSize(objIr), equals(2));
      expect(objectSize(nonObjIr), equals(0));
    });

    test('objectLookup', () {
      final objIr = IrObject({'key': IrString('value'), 'num': IrInteger(42)});
      final nonObjIr = IrList([]);

      expect(objectLookup('key', objIr), isA<IrString>());
      expect((objectLookup('key', objIr) as IrString).value, equals('value'));
      expect(objectLookup('missing', objIr), isNull);
      expect(objectLookup('key', nonObjIr), isNull);
    });

    test('isSymbol and getSymbol', () {
      final symbolIr = IrSymbol('x');
      final dottedIr = IrDottedSymbol(['a', 'b']);
      final nonSymbolIr = IrInteger(42);

      expect(isSymbol(symbolIr), isTrue);
      expect(isSymbol(dottedIr), isTrue);
      expect(isSymbol(nonSymbolIr), isFalse);

      expect(getSymbol(symbolIr), equals('x'));
      expect(getSymbol(dottedIr), equals('a.b'));
      expect(getSymbol(nonSymbolIr), equals(''));
    });
  });

  group('Compilation properties', () {
    test('list length preservation', () {
      final asts = [
        IntegerAst(1),
        StringAst('hello'),
        SymbolAst('x'),
        FloatAst(3.14),
      ];
      final ast = ListAst(asts);
      final ir = compile(ast);

      expect(isList(ir), isTrue);
      expect(listLength(ir), equals(asts.length));
    });

    test('object size properties', () {
      final props = {
        'a': IntegerAst(1),
        'b': StringAst('hello'),
        'c': SymbolAst('x'),
      };
      final ast = ObjectAst(props);
      final ir = compile(ast);

      expect(isObject(ir), isTrue);
      expect(objectSize(ir), equals(props.length));
    });

    test('symbol compilation idempotent', () {
      final symbols = ['x', 'hello', 'a.b.c', 'module.function'];
      for (final sym in symbols) {
        final ast = SymbolAst(sym);
        final ir = compile(ast);

        expect(isSymbol(ir), isTrue);
        expect(getSymbol(ir), equals(sym));
      }
    });

    test('recursive object lookup', () {
      final innerAst = IntegerAst(42);
      final ast = ObjectAst({
        'outer': ObjectAst({'inner': innerAst}),
      });
      final ir = compile(ast);

      expect(isObject(ir), isTrue);
      final outerObj = ir as IrObject;
      final innerIr = outerObj.properties['outer'];
      expect(innerIr, isA<IrObject>());

      final innerObj = innerIr as IrObject;
      final valueIr = innerObj.properties['inner'];
      expect(valueIr, isA<IrInteger>());
      expect((valueIr as IrInteger).value, equals(42));
    });
  });
}
