import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/lib/list.dart';
import 'package:glue/src/lib/builtin.dart';
import 'package:glue/src/lib/bool.dart';
import 'package:glue/src/lib/math/arithmetic/arithmetic.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<GlueError, Ir>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) async {
    final irTree = compile(ast);
    final env = envFromModules([
      builtin,
      bool,
      arithmetic,
      list,
    ]); // Load all necessary modules for testing
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.List.Partition (partition)', () {
    test('partitions list into matching and non-matching elements', () async {
      final result = await runCode(
        '(partition (lambda (x) (> x 3)) (1 2 3 4 5))',
      );
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrList([IrInteger(4), IrInteger(5)]),
              IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
            ]),
          ),
        ),
      );
    });

    test('partitions list with all elements matching', () async {
      final result = await runCode('(partition (lambda (x) (> x 0)) (1 2 3))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
              IrList([]),
            ]),
          ),
        ),
      );
    });

    test('partitions list with no elements matching', () async {
      final result = await runCode('(partition (lambda (x) (> x 10)) (1 2 3))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrList([]),
              IrList([IrInteger(1), IrInteger(2), IrInteger(3)]),
            ]),
          ),
        ),
      );
    });

    test('partitions empty list', () async {
      final result = await runCode('(partition (lambda (x) true) ())');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrList([]), IrList([])]))),
      );
    });

    test('partitions list with mixed matching', () async {
      final result = await runCode(
        '(partition (lambda (x) (== (% x 2) 0)) (1 2 3 4 5))',
      );
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrList([IrInteger(2), IrInteger(4)]),
              IrList([IrInteger(1), IrInteger(3), IrInteger(5)]),
            ]),
          ),
        ),
      );
    });

    test('fails on non-list second argument', () async {
      final result = await runCode('(partition (lambda (x) true) 42)');
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final result = await runCode('(partition (lambda (x) true))');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode(
        '(partition (lambda (x) true) (1 2 3) (4 5 6))',
      );
      expect(result.isLeft, isTrue);
    });
  });
}
