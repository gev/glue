import 'package:glue/compile.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/math/arithmetic.dart';
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
      arithmeticModule,
    ]); // Load only arithmetic module for testing
    final runtime = Runtime.initial(env);

    final evalResult = runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.Math.Arithmetic.Mod (%)', () {
    test('modulo with integers returns integer', () {
      final result = runCode('(% 10 3)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(1))),
      );
    });

    test('modulo with floats returns float', () {
      final result = runCode('(% 10.0 3.0)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(1.0, 0.0001)),
      );
    });

    test('modulo with mixed types returns float', () {
      final result = runCode('(% 10 3.0)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(1.0, 0.0001)),
      );
    });

    test('fails with division by zero', () {
      final result = runCode('(% 10 0)');
      expect(result.isLeft, isTrue);
    });

    test('fails with wrong argument types', () {
      final result = runCode('(% 5 "hello")');
      expect(result.isLeft, isTrue);
    });
  });
}
