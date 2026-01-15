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
  group('Glue.Lib.List.Zip (zip)', () {
    test('zips two lists of equal length', () async {
      final result = await runCode('(zip (1 2 3) ("a" "b" "c"))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrList([IrInteger(1), IrString('a')]),
              IrList([IrInteger(2), IrString('b')]),
              IrList([IrInteger(3), IrString('c')]),
            ]),
          ),
        ),
      );
    });

    test('zips two lists where first is shorter', () async {
      final result = await runCode('(zip (1 2) ("a" "b" "c"))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrList([IrInteger(1), IrString('a')]),
              IrList([IrInteger(2), IrString('b')]),
            ]),
          ),
        ),
      );
    });

    test('zips two lists where second is shorter', () async {
      final result = await runCode('(zip (1 2 3) ("a" "b"))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrList([IrInteger(1), IrString('a')]),
              IrList([IrInteger(2), IrString('b')]),
            ]),
          ),
        ),
      );
    });

    test('zips two empty lists', () async {
      final result = await runCode('(zip () ())');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([]))),
      );
    });

    test('zips one empty list with non-empty', () async {
      final result = await runCode('(zip () (1 2))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([]))),
      );
    });

    test('fails on non-list first argument', () async {
      final result = await runCode('(zip 42 (1 2))');
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list second argument', () async {
      final result = await runCode('(zip (1 2) 42)');
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final result = await runCode('(zip (1 2))');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode('(zip (1 2) (3 4) (5 6))');
      expect(result.isLeft, isTrue);
    });
  });
}
