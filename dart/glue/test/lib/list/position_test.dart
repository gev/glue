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
  group('Glue.Lib.List.Position (position)', () {
    test('finds position of first element that satisfies predicate', () async {
      final result = await runCode('(position (lambda (x) (> x 2)) (1 2 3 4))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(2))),
      );
    });

    test('finds position of first element in list', () async {
      final result = await runCode('(position (lambda (x) (> x 0)) (1 2 3))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(0))),
      );
    });

    test('finds position of element in middle of list', () async {
      final result = await runCode(
        '(position (lambda (x) (== x 5)) (1 2 5 3))',
      );
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(2))),
      );
    });

    test('fails when no element satisfies predicate', () async {
      final result = await runCode('(position (lambda (x) (> x 10)) (1 2 3))');
      expect(result.isLeft, isTrue);
    });

    test('fails on empty list', () async {
      final result = await runCode('(position (lambda (x) true) ())');
      expect(result.isLeft, isTrue);
    });

    test('fails on non-list second argument', () async {
      final result = await runCode('(position (lambda (x) true) 42)');
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong number of arguments', () async {
      final result = await runCode('(position (lambda (x) true))');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode(
        '(position (lambda (x) true) (1 2 3) (4 5 6))',
      );
      expect(result.isLeft, isTrue);
    });
  });
}
