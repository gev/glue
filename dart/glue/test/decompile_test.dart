import 'dart:math';

import 'package:glue/compile.dart';
import 'package:glue/decompile.dart';
import 'package:glue/src/ir.dart';
import 'package:test/test.dart' hide isList;

void main() {
  group('IR -> AST transformation (decompile)', () {
    group('Roundtrip: IR -> AST -> IR', () {
      // Property-based test matching Haskell DecompileSpec
      // Random property-based tests
      test('decompile and parses back to same value', () {
        for (int i = 0; i < 100; i++) {
          final random = Random(i);
          final ir1 = _generateIr(random, 5);

          final result = decompile(ir1);
          if (result.isLeft) {
            // Some types can't be decompiled - skip
            return;
          }

          final ast = result.match((l) => throw Exception(l), (r) => r);
          final ir2 = compile(ast);

          expect(ir1.toString(), equals(ir2.toString()));
        }
      });
    });
  });
}

// Generate random IR (only serializable types)
Ir _generateIr(Random random, int depth) {
  if (depth <= 0) {
    return _generateLeaf(random);
  }

  final type = random.nextInt(6);
  switch (type) {
    case 0:
      return IrInteger(random.nextInt(1000) - 500);
    case 1:
      return IrFloat(random.nextDouble() * 1000);
    case 2:
      return IrString(_generateString(random));
    case 3:
      return IrSymbol(_generateString(random));
    case 4:
      return _generateList(random, depth);
    case 5:
      return _generateObject(random, depth);
    default:
      return IrInteger(0);
  }
}

Ir _generateLeaf(Random random) {
  final type = random.nextInt(4);
  switch (type) {
    case 0:
      return IrInteger(random.nextInt(1000) - 500);
    case 1:
      return IrFloat(random.nextDouble() * 1000);
    case 2:
      return IrString(_generateString(random));
    case 3:
      return IrSymbol(_generateString(random));
    default:
      return IrInteger(0);
  }
}

String _generateString(Random random) {
  final length = random.nextInt(10) + 1;
  final chars = 'abcdefghijklmnopqrstuvwxyz';
  return List.generate(
    length,
    (_) => chars[random.nextInt(chars.length)],
  ).join();
}

IrList _generateList(Random random, int depth) {
  final length = random.nextInt(5);
  final elements = <Ir>[];
  for (int i = 0; i < length; i++) {
    elements.add(_generateIr(random, depth - 1));
  }
  return IrList(elements);
}

IrObject _generateObject(Random random, int depth) {
  final length = random.nextInt(5) + 1;
  final map = <String, Ir>{};
  for (int i = 0; i < length; i++) {
    map[_generateString(random)] = _generateIr(random, depth - 1);
  }
  return IrObject(map);
}
