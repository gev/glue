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
  group('Glue.Lib.List.Length (length)', () {
    test('returns 0 for empty list', () async {
      final result = await runCode('(length ())');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(0))),
      );
    });

    test('returns correct length for non-empty list', () async {
      final result = await runCode('(length (1 2 3))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(3))),
      );
    });

    test('returns length for list with mixed types', () async {
      final result = await runCode('(length (42 "hello" 3.14))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(3))),
      );
    });

    test('fails on non-list', () async {
      final result = await runCode('(length 42)');
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final result = await runCode('(length)');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode('(length (1 2 3) (4 5 6))');
      expect(result.isLeft, isTrue);
    });
  });
}
