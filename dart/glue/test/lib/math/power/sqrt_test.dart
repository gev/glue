import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/lib/math/power/power.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<GlueError, Ir>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) async {
    final irTree = compile(ast);
    final env = envFromModules([power]); // Load only power module for testing
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.Math.Power.Sqrt (sqrt)', () {
    test('sqrt with integer returns float', () async {
      final result = await runCode('(sqrt 4)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(2.0, 0.0001)),
      );
    });

    test('sqrt with float returns float', () async {
      final result = await runCode('(sqrt 9.0)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(3.0, 0.0001)),
      );
    });

    test('sqrt with perfect square', () async {
      final result = await runCode('(sqrt 16)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(4.0, 0.0001)),
      );
    });

    test('fails with wrong argument types', () async {
      final result = await runCode('(sqrt "hello")');
      expect(result.isLeft, isTrue);
    });

    test('fails with too few arguments', () async {
      final result = await runCode('(sqrt)');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode('(sqrt 4 5)');
      expect(result.isLeft, isTrue);
    });
  });
}
