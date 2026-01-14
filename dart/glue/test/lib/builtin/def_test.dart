import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/lib/builtin.dart';
import 'package:test/test.dart';
import 'package:glue/src/lib/builtin/def.dart';

void main() {
  group('Def Special Form', () {
    late Runtime runtime;

    setUp(() {
      final env = envFromModules([builtin]);
      runtime = Runtime.initial(env);
    });

    group('Defining variables', () {
      test('defines a variable in the environment', () async {
        final args = [IrSymbol('x'), IrInteger(42)];
        final result = await runEval(def(args), runtime);
        result.match((error) => fail('Def failed: $error'), (value) {
          final (res, runtime) = value;
          expect(res, equals(IrVoid()));
          // Check that x was defined
          final lookupResult = lookupVar('x', runtime.env);
          expect(lookupResult.isRight, isTrue);
          lookupResult.match(
            (error) => fail('Lookup failed: $error'),
            (val) => expect(val, equals(IrInteger(42))),
          );
        });
      });

      test('fails with wrong number of arguments', () async {
        final args = [IrSymbol('x')];
        final result = await runEval(def(args), runtime);
        expect(result.isLeft, isTrue);
      });

      test('fails with non-symbol as name', () async {
        final args = [IrInteger(1), IrInteger(42)];
        final result = await runEval(def(args), runtime);
        expect(result.isLeft, isTrue);
      });
    });

    group('Function definition sugar', () {
      test('defines simple function', () async {
        final args = [
          IrList([IrSymbol('square'), IrSymbol('x')]),
          IrList([IrSymbol('*'), IrSymbol('x'), IrSymbol('x')]),
        ];
        final result = await runEval(def(args), runtime);
        result.match((error) => fail('Def failed: $error'), (value) {
          final (res, runtime) = value;
          // Should return the closure
          expect(res, isA<IrClosure>());
          final closure = res as IrClosure;
          expect(closure.params, equals(['x']));

          // Check that square function was also defined
          final lookupResult = lookupVar('square', runtime.env);
          expect(lookupResult.isRight, isTrue);
          lookupResult.match((error) => fail('Lookup failed: $error'), (val) {
            expect(val, isA<IrClosure>());
            final squareClosure = val as IrClosure;
            expect(squareClosure.params, equals(['x']));
          });
        });
      });

      test('defines function with multiple parameters', () async {
        final args = [
          IrList([IrSymbol('add'), IrSymbol('x'), IrSymbol('y')]),
          IrList([IrSymbol('+'), IrSymbol('x'), IrSymbol('y')]),
        ];
        final result = await runEval(def(args), runtime);
        result.match((error) => fail('Def failed: $error'), (value) {
          final (res, runtime) = value;
          // Should return the closure
          expect(res, isA<IrClosure>());
          final closure = res as IrClosure;
          expect(closure.params, equals(['x', 'y']));

          // Check that add function was also defined
          final lookupResult = lookupVar('add', runtime.env);
          expect(lookupResult.isRight, isTrue);
          lookupResult.match((error) => fail('Lookup failed: $error'), (val) {
            expect(val, isA<IrClosure>());
            final addClosure = val as IrClosure;
            expect(addClosure.params, equals(['x', 'y']));
          });
        });
      });

      test('defines function with multiple body expressions', () async {
        final args = [
          IrList([IrSymbol('test'), IrSymbol('x')]),
          IrList([IrSymbol('println'), IrString('hello')]),
          IrList([IrSymbol('*'), IrSymbol('x'), IrInteger(2)]),
        ];
        final result = await runEval(def(args), runtime);
        result.match((error) => fail('Def failed: $error'), (value) {
          final (res, runtime) = value;
          // Should return the closure
          expect(res, isA<IrClosure>());
          final closure = res as IrClosure;
          expect(closure.params, equals(['x']));

          // Check that test function was also defined
          final lookupResult = lookupVar('test', runtime.env);
          expect(lookupResult.isRight, isTrue);
          lookupResult.match((error) => fail('Lookup failed: $error'), (val) {
            expect(val, isA<IrClosure>());
            final testClosure = val as IrClosure;
            expect(testClosure.params, equals(['x']));
          });
        });
      });

      test('fails with invalid function signature', () async {
        final args = [
          IrList([IrInteger(42), IrSymbol('x')]),
          IrList([IrSymbol('*'), IrSymbol('x'), IrSymbol('x')]),
        ];
        final result = await runEval(def(args), runtime);
        expect(result.isLeft, isTrue);
      });
    });
  });
}
