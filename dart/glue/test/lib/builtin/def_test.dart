import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/builtin.dart';
import 'package:glue/src/lib/builtin/def.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';

void main() {
  group('Def Special Form', () {
    late Runtime runtime;

    setUp(() {
      final env = envFromModules([builtinModule]);
      runtime = Runtime.initial(env);
    });

    group('Defining variables', () {
      test('defines a variable in the environment', () {
        final args = [IrSymbol('x'), IrInteger(42)];
        final result = runEval(apply(def, args), runtime);
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

      test('fails with non-symbol as name', () {
        final args = [IrInteger(1), IrInteger(42)];
        final result = runEval(apply(def, args), runtime);
        expect(result.isLeft, isTrue);
      });
    });

    group('Function definition sugar', () {
      test('defines simple function', () {
        final args = [
          IrList([IrSymbol('square'), IrSymbol('x')]),
          IrList([IrSymbol('*'), IrSymbol('x'), IrSymbol('x')]),
        ];
        final result = runEval(apply(def, args), runtime);
        result.match((error) => fail('Def failed: $error'), (value) {
          final (res, runtime) = value;
          // Should return IrVoid like variable definitions
          expect(res, equals(IrVoid()));

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

      test('defines function with multiple parameters', () {
        final args = [
          IrList([IrSymbol('add'), IrSymbol('x'), IrSymbol('y')]),
          IrList([IrSymbol('+'), IrSymbol('x'), IrSymbol('y')]),
        ];
        final result = runEval(apply(def, args), runtime);
        result.match((error) => fail('Def failed: $error'), (value) {
          final (res, runtime) = value;
          // Should return IrVoid like variable definitions
          expect(res, equals(IrVoid()));

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

      test('defines function with multiple body expressions', () {
        final args = [
          IrList([IrSymbol('test'), IrSymbol('x')]),
          IrList([IrSymbol('println'), IrString('hello')]),
          IrList([IrSymbol('*'), IrSymbol('x'), IrInteger(2)]),
        ];
        final result = runEval(apply(def, args), runtime);
        result.match((error) => fail('Def failed: $error'), (value) {
          final (res, runtime) = value;
          // Should return IrVoid like variable definitions
          expect(res, equals(IrVoid()));

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

      test('fails with invalid function signature', () {
        final args = [
          IrList([IrInteger(42), IrSymbol('x')]),
          IrList([IrSymbol('*'), IrSymbol('x'), IrSymbol('x')]),
        ];
        final result = runEval(apply(def, args), runtime);
        expect(result.isLeft, isTrue);
      });
    });
  });
}
