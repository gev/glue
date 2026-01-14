import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';
import 'package:glue/src/lib/builtin/set.dart';

void main() {
  group('Set Special Form', () {
    late Runtime runtime;

    setUp(() {
      final env = fromList([]);
      runtime = Runtime.initial(env);
    });

    group('Updating variables', () {
      test('updates an existing variable', () async {
        final initialEnv = fromList([('x', IrInteger(10))]);
        final testRuntime = Runtime.initial(initialEnv);
        final args = [IrSymbol('x'), IrInteger(20)];

        final result = await runEval(set(args), testRuntime);

        expect(result.isRight, isTrue);
        result.match((error) => fail('Set failed: $error'), (value) {
          final (res, runtime) = value;
          expect(res, equals(IrVoid()));
          final lookupResult = lookupVar('x', runtime.env);
          expect(lookupResult.isRight, isTrue);
          lookupResult.match(
            (error) => fail('Lookup failed: $error'),
            (val) => expect(val, equals(IrInteger(20))),
          );
        });
      });

      test('fails to set unbound variable', () async {
        final args = [IrSymbol('x'), IrInteger(42)];
        final result = await runEval(set(args), runtime);
        expect(result.isLeft, isTrue);
      });
    });

    group('Setting object properties', () {
      test('sets a property on an object', () async {
        final obj = IrObject({'a': IrInteger(1)});
        final initialEnv = fromList([('obj', obj)]);
        final testRuntime = Runtime.initial(initialEnv);
        final args = [IrSymbol('obj.b'), IrInteger(2)];

        final result = await runEval(set(args), testRuntime);

        expect(result.isRight, isTrue);
        result.match((error) => fail('Set failed: $error'), (value) {
          final (res, runtime) = value;
          expect(res, equals(IrVoid()));

          final lookupResult = lookupVar('obj', runtime.env);
          expect(lookupResult.isRight, isTrue);
          lookupResult.match((error) => fail('Lookup failed: $error'), (obj) {
            expect(obj, isA<IrObject>());
            final irObj = obj as IrObject;
            expect(irObj.properties['a'], equals(IrInteger(1)));
            expect(irObj.properties['b'], equals(IrInteger(2)));
          });
        });
      });

      test('fails to set property on non-object', () async {
        final initialEnv = fromList([('x', IrInteger(10))]);
        final testRuntime = Runtime.initial(initialEnv);
        final args = [IrSymbol('x.prop'), IrInteger(42)];

        final result = await runEval(set(args), testRuntime);
        expect(result.isLeft, isTrue);
      });
    });

    group('Error cases', () {
      test('fails with wrong number of arguments', () async {
        final args = [IrSymbol('x')];
        final result = await runEval(set(args), runtime);
        expect(result.isLeft, isTrue);
      });
    });
  });
}
