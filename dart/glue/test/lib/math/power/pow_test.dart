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
  group('Glue.Lib.Math.Power.Pow (pow)', () {
    test('integer base and exponent returns integer', () async {
      final result = await runCode('(pow 2 3)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(8))),
      );
    });

    test('integer base, float exponent returns float', () async {
      final result = await runCode('(pow 2 0.5)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) =>
            expect((value as IrFloat).value, closeTo(1.414213562, 0.0001)),
      );
    });

    test('float base, integer exponent returns float', () async {
      final result = await runCode('(pow 2.0 3)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(8.0, 0.0001)),
      );
    });

    test('float base and exponent returns float', () async {
      final result = await runCode('(pow 2.0 0.5)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) =>
            expect((value as IrFloat).value, closeTo(1.414213562, 0.0001)),
      );
    });

    test('fails with wrong argument types', () async {
      final result = await runCode('(pow 2 "hello")');
      expect(result.isLeft, isTrue);
    });

    test('fails with too few arguments', () async {
      final result = await runCode('(pow 2)');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode('(pow 2 3 4)');
      expect(result.isLeft, isTrue);
    });
  });
}
