import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/glue.dart';
import 'package:test/test.dart';

void main() {
  group('AST', () {
    test('StringAst equality and toString', () {
      final ast1 = StringAst('hello');
      final ast2 = StringAst('hello');
      final ast3 = StringAst('world');

      expect(ast1, equals(ast2));
      expect(ast1, isNot(equals(ast3)));
      expect(ast1.toString(), equals('"hello"'));
    });

    test('IntegerAst equality and toString', () {
      final ast1 = IntegerAst(42);
      final ast2 = IntegerAst(42);
      final ast3 = IntegerAst(24);

      expect(ast1, equals(ast2));
      expect(ast1, isNot(equals(ast3)));
      expect(ast1.toString(), equals('42'));
    });

    test('FloatAst equality and toString', () {
      final ast1 = FloatAst(3.14);
      final ast2 = FloatAst(3.14);
      final ast3 = FloatAst(2.71);

      expect(ast1, equals(ast2));
      expect(ast1, isNot(equals(ast3)));
      expect(ast1.toString(), equals('3.14'));
    });

    test('SymbolAst equality and toString', () {
      final ast1 = SymbolAst('x');
      final ast2 = SymbolAst('x');
      final ast3 = SymbolAst('y');

      expect(ast1, equals(ast2));
      expect(ast1, isNot(equals(ast3)));
      expect(ast1.toString(), equals('x'));
    });

    test('ListAst with immutable collections', () {
      final elements1 = IList<Ast>([IntegerAst(1), StringAst('hello')]);
      final elements2 = IList<Ast>([IntegerAst(1), StringAst('hello')]);
      final elements3 = IList<Ast>([IntegerAst(1), StringAst('world')]);

      final ast1 = ListAst(elements1);
      final ast2 = ListAst(elements2);
      final ast3 = ListAst(elements3);

      expect(ast1, equals(ast2));
      expect(ast1, isNot(equals(ast3)));
      expect(ast1.toString(), equals('(1 "hello")'));
    });

    test('ObjectAst with immutable collections', () {
      final props1 = IMap<String, Ast>(
          {'name': StringAst('Alice'), 'age': IntegerAst(30)});
      final props2 = IMap<String, Ast>(
          {'name': StringAst('Alice'), 'age': IntegerAst(30)});
      final props3 =
          IMap<String, Ast>({'name': StringAst('Bob'), 'age': IntegerAst(25)});

      final ast1 = ObjectAst(props1);
      final ast2 = ObjectAst(props2);
      final ast3 = ObjectAst(props3);

      expect(ast1, equals(ast2));
      expect(ast1, isNot(equals(ast3)));
      expect(ast1.toString(), equals('(:name "Alice" :age 30)'));
    });

    test('AST pattern matching (sealed class)', () {
      final ast = StringAst('test');

      final result = switch (ast) {
        StringAst(:final value) => 'string: $value',
        IntegerAst(:final value) => 'int: $value',
        FloatAst(:final value) => 'float: $value',
        SymbolAst(:final value) => 'symbol: $value',
        ListAst(:final elements) => 'list: ${elements.length} elements',
        ObjectAst(:final properties) =>
          'object: ${properties.length} properties',
      };

      expect(result, equals('string: test'));
    });

    test('Hash codes for collections', () {
      final list1 = ListAst(IList([IntegerAst(1), IntegerAst(2)]));
      final list2 = ListAst(IList([IntegerAst(1), IntegerAst(2)]));
      final list3 = ListAst(IList([IntegerAst(2), IntegerAst(1)]));

      expect(list1.hashCode, equals(list2.hashCode));
      expect(list1.hashCode, isNot(equals(list3.hashCode)));
    });
  });
}
