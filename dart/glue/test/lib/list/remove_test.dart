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
  group('Glue.Lib.List.Remove (remove)', () {
    test('removes item from list', () async {
      final result = await runCode('(remove 2 (1 2 3))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrInteger(1), IrInteger(3)]))),
      );
    });

    test('removes all occurrences of item', () async {
      final result = await runCode('(remove 2 (1 2 2 3))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrInteger(1), IrInteger(3)]))),
      );
    });

    test('returns same list if item not found', () async {
      final result = await runCode('(remove 4 (1 2 3))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(IrList([IrInteger(1), IrInteger(2), IrInteger(3)])),
        ),
      );
    });

    test('removes from empty list', () async {
      final result = await runCode('(remove 1 ())');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([]))),
      );
    });

    test('fails on non-list second argument', () async {
      final result = await runCode('(remove 1 42)');
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final result = await runCode('(remove 1)');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode('(remove 1 (1 2 3) (4 5 6))');
      expect(result.isLeft, isTrue);
    });
  });
}
