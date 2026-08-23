import 'package:glue/compile.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/math/logarithmic.dart';
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
      logarithmicModule,
    ]); // Load only logarithmic module for testing
    final runtime = Runtime.initial(env);

    final evalResult = runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.Math.Logarithmic.Log (arbitrary base logarithm)', () {
    test('log with integer base and value returns float', () {
      final result = runCode('(log 8 2)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(3.0, 0.0001)),
      );
    });

    test('log with float base and value returns float', () {
      final result = runCode('(log 9.0 3.0)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(2.0, 0.0001)),
      );
    });

    test('log with mixed types returns float', () {
      final result = runCode('(log 16 2.0)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(4.0, 0.0001)),
      );
    });

    test('log base 10 equals lg', () {
      final result1 = runCode('(log 100 10)');
      final result2 = runCode('(lg 100)');

      result1.match(
        (error) => fail('Should not be left: $error'),
        (value1) => result2.match(
          (error) => fail('Should not be left: $error'),
          (value2) => expect(
            (value1 as IrFloat).value,
            closeTo((value2 as IrFloat).value, 0.0001),
          ),
        ),
      );
    });

    test('fails with wrong argument types', () {
      final result = runCode('(log 8 "hello")');
      expect(result.isLeft, isTrue);
    });
  });
}
