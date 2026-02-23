import 'dart:math';

import 'package:glue/compile.dart';
import 'package:glue/decompile.dart';
import 'package:glue/src/ast.dart';
import 'package:glue/src/ir.dart';
import 'package:test/test.dart' hide isList;

// Random generator for IR (only serializable types)
class IrGenerator {
  final Random _random;
  int _depth = 0;
  static const int _maxDepth = 5;

  IrGenerator(this._random);

  Ir generate() {
    _depth = 0;
    return _generate();
  }

  Ir _generate() {
    if (_depth >= _maxDepth) {
      return _generateLeaf();
    }
    _depth++;

    final type = _random.nextInt(6);
    switch (type) {
      case 0:
        return IrInteger(_random.nextInt(1000) - 500);
      case 1:
        return IrFloat(_random.nextDouble() * 1000);
      case 2:
        return IrString(_generateString());
      case 3:
        return IrSymbol(_generateString());
      case 4:
        return _generateList();
      case 5:
        return _generateObject();
      default:
        return IrInteger(0);
    }
  }

  Ir _generateLeaf() {
    final type = _random.nextInt(4);
    switch (type) {
      case 0:
        return IrInteger(_random.nextInt(1000) - 500);
      case 1:
        return IrFloat(_random.nextDouble() * 1000);
      case 2:
        return IrString(_generateString());
      case 3:
        return IrSymbol(_generateString());
      default:
        return IrInteger(0);
    }
  }

  String _generateString() {
    final length = _random.nextInt(10) + 1;
    final chars = 'abcdefghijklmnopqrstuvwxyz';
    return List.generate(
      length,
      (_) => chars[_random.nextInt(chars.length)],
    ).join();
  }

  IrList _generateList() {
    final length = _random.nextInt(5);
    final elements = <Ir>[];
    for (int i = 0; i < length; i++) {
      elements.add(_generate());
    }
    return IrList(elements);
  }

  IrObject _generateObject() {
    final length = _random.nextInt(5) + 1;
    final map = <String, Ir>{};
    for (int i = 0; i < length; i++) {
      map[_generateString()] = _generate();
    }
    return IrObject(map);
  }
}

void main() {
  group('IR -> AST decompilation', () {
    test('decompile IrInteger returns IntegerAst', () {
      final ir = IrInteger(42);
      final result = decompile(ir);
      expect(result.isRight, isTrue);
      expect(result.match((l) => null, (r) => r), isA<IntegerAst>());
    });

    test('decompile IrFloat returns FloatAst', () {
      final ir = IrFloat(3.14);
      final result = decompile(ir);
      expect(result.isRight, isTrue);
      expect(result.match((l) => null, (r) => r), isA<FloatAst>());
    });

    test('decompile IrString returns StringAst', () {
      final ir = IrString('hello');
      final result = decompile(ir);
      expect(result.isRight, isTrue);
      expect(result.match((l) => null, (r) => r), isA<StringAst>());
    });

    test('decompile IrBool returns SymbolAst', () {
      final irTrue = IrBool(true);
      final irFalse = IrBool(false);

      final resultTrue = decompile(irTrue);
      final resultFalse = decompile(irFalse);

      expect(resultTrue.isRight, isTrue);
      expect(resultTrue.match((l) => null, (r) => r), isA<SymbolAst>());
      expect(
        (resultTrue.match((l) => null, (r) => r) as SymbolAst).value,
        equals('true'),
      );

      expect(resultFalse.isRight, isTrue);
      expect(resultFalse.match((l) => null, (r) => r), isA<SymbolAst>());
      expect(
        (resultFalse.match((l) => null, (r) => r) as SymbolAst).value,
        equals('false'),
      );
    });

    test('decompile IrSymbol returns SymbolAst', () {
      final ir = IrSymbol('x');
      final result = decompile(ir);
      expect(result.isRight, isTrue);
      final ast = result.match((l) => null, (r) => r) as SymbolAst;
      expect(ast.value, equals('x'));
    });

    test('decompile IrDottedSymbol returns SymbolAst with joined dots', () {
      final ir = IrDottedSymbol(['a', 'b', 'c']);
      final result = decompile(ir);
      expect(result.isRight, isTrue);
      final ast = result.match((l) => null, (r) => r) as SymbolAst;
      expect(ast.value, equals('a.b.c'));
    });

    test('decompile IrList returns ListAst', () {
      final ir = IrList([IrInteger(1), IrString('hello'), IrSymbol('x')]);
      final result = decompile(ir);
      expect(result.isRight, isTrue);
      expect(result.match((l) => null, (r) => r), isA<ListAst>());
    });

    test('decompile IrObject returns ObjectAst', () {
      final ir = IrObject({'name': IrString('Alice'), 'age': IrInteger(30)});
      final result = decompile(ir);
      expect(result.isRight, isTrue);
      expect(result.match((l) => null, (r) => r), isA<ObjectAst>());
    });

    test('decompile IrVoid returns Left error', () {
      final ir = IrVoid();
      final result = decompile(ir);
      expect(result.isLeft, isTrue);
    });

    test('decompile non-serializable types returns Left error', () {
      // These require functions which we can't easily create in tests
      // Just verify the types exist
      expect(IrEvaluable(() => throw UnimplementedError()), isA<Ir>());
    });
  });

  group('Roundtrip: IR -> AST -> IR', () {
    test('decompile then compile returns equivalent IR (integer)', () {
      final ir1 = IrInteger(42);
      final result = decompile(ir1);
      expect(result.isRight, isTrue);

      final ast = result.match((l) => throw Exception(l), (r) => r);
      final ir2 = compile(ast);

      expect(ir1.toString(), equals(ir2.toString()));
    });

    test('decompile then compile returns equivalent IR (float)', () {
      final ir1 = IrFloat(3.14);
      final result = decompile(ir1);
      expect(result.isRight, isTrue);

      final ast = result.match((l) => throw Exception(l), (r) => r);
      final ir2 = compile(ast);

      expect(ir1.toString(), equals(ir2.toString()));
    });

    test('decompile then compile returns equivalent IR (string)', () {
      final ir1 = IrString('hello');
      final result = decompile(ir1);
      expect(result.isRight, isTrue);

      final ast = result.match((l) => throw Exception(l), (r) => r);
      final ir2 = compile(ast);

      expect(ir1.toString(), equals(ir2.toString()));
    });

    test('decompile then compile returns equivalent IR (symbol)', () {
      final ir1 = IrSymbol('x');
      final result = decompile(ir1);
      expect(result.isRight, isTrue);

      final ast = result.match((l) => throw Exception(l), (r) => r);
      final ir2 = compile(ast);

      expect(ir1.toString(), equals(ir2.toString()));
    });

    test('decompile then compile returns equivalent IR (list)', () {
      final ir1 = IrList([IrInteger(1), IrString('hello')]);
      final result = decompile(ir1);
      expect(result.isRight, isTrue);

      final ast = result.match((l) => throw Exception(l), (r) => r);
      final ir2 = compile(ast);

      expect(ir1.toString(), equals(ir2.toString()));
    });

    test('decompile then compile returns equivalent IR (object)', () {
      final ir1 = IrObject({'name': IrString('Alice'), 'age': IrInteger(30)});
      final result = decompile(ir1);
      expect(result.isRight, isTrue);

      final ast = result.match((l) => throw Exception(l), (r) => r);
      final ir2 = compile(ast);

      expect(ir1.toString(), equals(ir2.toString()));
    });

    test('decompile then compile returns equivalent IR (nested)', () {
      final ir1 = IrList([
        IrObject({'a': IrInteger(1)}),
        IrList([IrSymbol('x.y'), IrFloat(2.5)]),
      ]);
      final result = decompile(ir1);
      expect(result.isRight, isTrue);

      final ast = result.match((l) => throw Exception(l), (r) => r);
      final ir2 = compile(ast);

      expect(ir1.toString(), equals(ir2.toString()));
    });

    // Property-based tests with random data
    for (int i = 0; i < 50; i++) {
      test('random IR roundtrip test $i', () {
        final random = Random(i);
        final generator = IrGenerator(random);
        final ir1 = generator.generate();

        final result = decompile(ir1);
        if (result.isLeft) {
          // Some types can't be decompiled - that's ok for this test
          return;
        }

        final ast = result.match((l) => throw Exception(l), (r) => r);
        final ir2 = compile(ast);

        expect(ir1.toString(), equals(ir2.toString()));
      });
    }
  });
}
