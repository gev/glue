import 'dart:math' as math;

import 'package:glue/src/either.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/lib/math/const.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<EvalError, Ir?>> runCode(List<Ir> args) async {
  final env = envFromModules([const_]); // Load only const module for testing
  final runtime = Runtime.initial(env);

  final evalResult = await runEval(eval(IrSymbol('pi')), runtime);
  return evalResult.match((error) => Left(error), (value) {
    final (result, _) = value;
    return Right(result);
  });
}

void main() {
  group('Glue.Lib.Math.Const (Test math constants)', () {
    group('Mathematical constants', () {
      test('pi constant matches Dart math.pi', () async {
        final env = envFromModules([const_]);
        final runtime = Runtime.initial(env);

        final evalResult = await runEval(eval(IrSymbol('pi')), runtime);
        evalResult.match((error) => fail('Should not be left: $error'), (
          value,
        ) {
          final (result, _) = value;
          expect(result, equals(IrFloat(math.pi)));
        });
      });

      test('e constant matches Dart math.e', () async {
        final env = envFromModules([const_]);
        final runtime = Runtime.initial(env);

        final evalResult = await runEval(eval(IrSymbol('e')), runtime);
        evalResult.match((error) => fail('Should not be left: $error'), (
          value,
        ) {
          final (result, _) = value;
          expect(result, equals(IrFloat(math.e)));
        });
      });
    });
  });
}
