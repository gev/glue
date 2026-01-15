import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/lib/list.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<GlueError, Ir>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) async {
    final irTree = compile(ast);
    final env = envFromModules([list]); // Load only list module for testing
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.List.Butlast (butlast)', () {
    test('returns all elements except the last one', () async {
      final result = await runCode('(butlast (1 2 3))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrInteger(1), IrInteger(2)]))),
      );
    });

    test('returns empty list for single-element list', () async {
      final result = await runCode('(butlast (42))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([]))),
      );
    });

    test('returns string elements except last', () async {
      final result = await runCode('(butlast ("hello" "world" "test"))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(IrList([IrString('hello'), IrString('world')])),
        ),
      );
    });

    test('fails on empty list', () async {
      final result = await runCode('(butlast ())');
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list argument', () async {
      final result = await runCode('(butlast 42)');
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final result = await runCode('(butlast)');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode('(butlast (1 2 3) (4 5 6))');
      expect(result.isLeft, isTrue);
    });
  });
}
