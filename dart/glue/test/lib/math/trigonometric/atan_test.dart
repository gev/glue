import 'package:glue/compile.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/math/trigonometric.dart';
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
      trigonometricModule,
    ]); // Load only trigonometric module for testing
    final runtime = Runtime.initial(env);

    final evalResult = runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.Math.Trigonometric.Atan (atan)', () {
    test('atan with common angles', () {
      final result = runCode('(atan 0)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(0.0, 0.0001)),
      );
    });

    test('atan with integer input', () {
      final result = runCode('(atan 1)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, isA<IrFloat>()),
      );
    });

    test('atan with float input', () {
      final result = runCode('(atan 1.0)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, isA<IrFloat>()),
      );
    });

    test('fails with wrong argument types', () {
      final result = runCode('(atan "hello")');
      expect(result.isLeft, isTrue);
    });
  });
}
