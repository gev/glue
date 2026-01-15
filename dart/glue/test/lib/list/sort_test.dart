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
  group('Glue.Lib.List.Sort (sort)', () {
    test('sorts a list of numbers in ascending order', () async {
      final result = await runCode('(sort (3 1 4 1 5))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrInteger(1),
              IrInteger(1),
              IrInteger(3),
              IrInteger(4),
              IrInteger(5),
            ]),
          ),
        ),
      );
    });

    test('sorts a list of strings in alphabetical order', () async {
      final result = await runCode('(sort ("zebra" "apple" "banana"))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([IrString('apple'), IrString('banana'), IrString('zebra')]),
          ),
        ),
      );
    });

    test('sorts an empty list', () async {
      final result = await runCode('(sort ())');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([]))),
      );
    });

    test('sorts a single element list', () async {
      final result = await runCode('(sort (42))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrInteger(42)]))),
      );
    });

    test('sorts a list with duplicate elements', () async {
      final result = await runCode('(sort (3 1 3 1 2))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrInteger(1),
              IrInteger(1),
              IrInteger(2),
              IrInteger(3),
              IrInteger(3),
            ]),
          ),
        ),
      );
    });

    test('fails on non-list argument', () async {
      final result = await runCode('(sort 42)');
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final result = await runCode('(sort)');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode('(sort (1 2 3) (4 5 6))');
      expect(result.isLeft, isTrue);
    });

    test('fails on list with incomparable elements', () async {
      final result = await runCode('(sort (1 (2)))');
      expect(result.isLeft, isTrue);
    });
  });
}
