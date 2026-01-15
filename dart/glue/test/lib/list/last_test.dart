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
  group('Glue.Lib.List.Last (last)', () {
    test('returns the last element of a list', () async {
      final result = await runCode('(last (1 2 3))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(3))),
      );
    });

    test('returns the only element of a single-element list', () async {
      final result = await runCode('(last (42))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );
    });

    test('returns string element', () async {
      final result = await runCode('(last ("hello" "world"))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('world'))),
      );
    });

    test('fails on empty list', () async {
      final result = await runCode('(last ())');
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list argument', () async {
      final result = await runCode('(last 42)');
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final result = await runCode('(last)');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode('(last (1 2 3) (4 5 6))');
      expect(result.isLeft, isTrue);
    });
  });
}
