import 'package:glue/compile.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/list.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/parse.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Either<GlueError, Ir> runCode(String input) {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) {
    final irTree = compile(ast);
    final env = envFromModules([
      listModule,
    ]); // Load only list module for testing
    final runtime = Runtime.initial(env);

    final evalResult = runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.List.Flatten (flatten)', () {
    test('flattens a simple nested list', () {
      final result = runCode('(flatten ((1 2) (3 4)))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([IrInteger(1), IrInteger(2), IrInteger(3), IrInteger(4)]),
          ),
        ),
      );
    });

    test('flattens deeply nested lists', () {
      final result = runCode('(flatten (((1) 2) 3))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(IrList([IrInteger(1), IrInteger(2), IrInteger(3)])),
        ),
      );
    });

    test('flattens list with mixed elements', () {
      final result = runCode('(flatten (1 (2 3) 4))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([IrInteger(1), IrInteger(2), IrInteger(3), IrInteger(4)]),
          ),
        ),
      );
    });

    test('flattens empty list', () {
      final result = runCode('(flatten ())');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([]))),
      );
    });

    test('flattens list with empty sublists', () {
      final result = runCode('(flatten (() 1 ()))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrInteger(1)]))),
      );
    });

    test('flattens single element list', () {
      final result = runCode('(flatten (42))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrInteger(42)]))),
      );
    });

    test('fails on non-list argument', () {
      final result = runCode('(flatten 42)');
      expect(result.isLeft, isTrue);
    });
  });
}
