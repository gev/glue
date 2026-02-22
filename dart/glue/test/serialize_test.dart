import 'dart:math';

import 'package:glue/ast.dart';
import 'package:glue/parse.dart';
import 'package:glue/serialize.dart';
import 'package:test/test.dart';

/// Valid symbol chars: letter first, then letter/digit/special
final _letterChars = 'abcdefghijklmnopqrstuvwxyz';
final _digitChars = '0123456789';
final _specialChars = '+-*/%=<>&|\!?\$@#_.\'';

String _genValidSymbol(Random random) {
  final first = _letterChars[random.nextInt(_letterChars.length)];
  final rest = List.generate(random.nextInt(5) + 1, (_) {
    final allChars = _letterChars + _digitChars + _specialChars;
    return allChars[random.nextInt(allChars.length)];
  }).join();
  return first + rest;
}

/// Generate printable ASCII for strings
String _genValidString(Random random) {
  final chars =
      'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 +-.,/:;!?@#\$%&*_';
  return List.generate(
    random.nextInt(10),
    (_) => chars[random.nextInt(chars.length)],
  ).join();
}

/// Generate arbitrary Ast for testing
Ast _genAst(Random random, int depth) {
  if (depth <= 0) {
    final idx = random.nextInt(4);
    switch (idx) {
      case 0:
        return SymbolAst(_genValidSymbol(random));
      case 1:
        return IntegerAst(random.nextInt(100) - 50);
      case 2:
        return FloatAst(random.nextDouble() * 100 - 50);
      case 3:
        return StringAst(_genValidString(random));
      default:
        return IntegerAst(0);
    }
  }

  final idx = random.nextInt(6);
  switch (idx) {
    case 0:
      return SymbolAst(_genValidSymbol(random));
    case 1:
      return IntegerAst(random.nextInt(100) - 50);
    case 2:
      return FloatAst(random.nextDouble() * 100 - 50);
    case 3:
      return StringAst(_genValidString(random));
    case 4:
      // List
      final len = random.nextInt(3) + 1;
      return ListAst(List.generate(len, (_) => _genAst(random, depth - 1)));
    case 5:
      // Object
      final len = random.nextInt(3) + 1;
      final props = <String, Ast>{};
      for (var i = 0; i < len; i++) {
        props[_genValidSymbol(random)] = _genAst(random, depth - 1);
      }
      return ObjectAst(props);
    default:
      return IntegerAst(0);
  }
}

void main() {
  group('Glue Serialize', () {
    group('Roundtrip: AST -> Text -> AST', () {
      test('serializes and parses back to same value', () {
        final random = Random(42);
        for (var i = 0; i < 100; i++) {
          final ast = _genAst(random, 3);
          final text = serializeAst(ast);
          final parsed = parseGlue(text);
          expect(parsed.isRight, true);
          parsed.match(
            (error) => fail('Expected success, got error: $error'),
            (result) => expect(result, equals(ast)),
          );
        }
      });
    });
  });
}
