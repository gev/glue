import 'package:glue/src/env.dart';
import 'package:glue/src/ir.dart' hide Env;
import 'package:glue/src/runtime_exceptions.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Env (Test stack memory model)', () {
    group('Smart constructors and atomic operations', () {
      test('emptyEnv: create empty stack environment', () {
        final env = emptyEnv();
        expect(lookupLocal('any', env), isNull);
        final (error, value) = lookupVar('any', env);
        expect(error, isA<RuntimeException>());
        expect(error?.symbol, equals('unbound-variable'));
        expect(value, isNull);
      });

      test('fromList: initialize environment stack', () {
        final env = fromList([('a', IrInteger(1)), ('b', IrInteger(2))]);
        expect(lookupLocal('a', env), equals(IrInteger(1)));
        final (error, value) = lookupVar('b', env);
        expect(error, isNull);
        expect(value, equals(IrInteger(2)));
      });

      test('pushFrame / popFrame: manage stack (LIFO)', () {
        final base = fromList([('x', IrInteger(1))]);
        final pushed = pushFrame(base);
        expect(lookupLocal('x', pushed), isNull);
        final (error, value) = lookupVar('x', pushed);
        expect(error, isNull);
        expect(value, equals(IrInteger(1)));
        expect(popFrame(pushed), equals(base));
      });

      test('popFrame don\'t crash on empty stack', () {
        expect(popFrame(emptyEnv()), equals(emptyEnv()));
      });
    });

    group('Define and search (shadowing)', () {
      test('defineVar: always define at the top frame', () {
        final env = pushFrame(fromList([('other', IrInteger(0))]));
        final newEnv = defineVar('name', IrInteger(42), env);
        expect(lookupLocal('name', newEnv), equals(IrInteger(42)));
      });

      test('Shadowing: local definition shadow global', () {
        final env = fromList([('name', IrInteger(1))]);
        final finalEnv = defineVar('name', IrInteger(2), pushFrame(env));
        final (error, value) = lookupVar('name', finalEnv);
        expect(error, isNull);
        expect(value, equals(IrInteger(2)));
        final (error2, value2) = lookupVar('name', popFrame(finalEnv));
        expect(error2, isNull);
        expect(value2, equals(IrInteger(1)));
      });
    });

    group('Variable updating', () {
      test(
        'updateVar: update values in the place, don\'t create a new one',
        () {
          final env = pushFrame(fromList([('x', IrInteger(10))]));
          final (error, updatedEnv) = updateVar('x', IrInteger(20), env);
          expect(error, isNull);
          expect(updatedEnv, isNotNull);

          expect(lookupLocal('x', updatedEnv!), isNull);
          final (error2, value) = lookupVar('x', updatedEnv);
          expect(error2, isNull);
          expect(value, equals(IrInteger(20)));
        },
      );

      test('updateVar: return error for unbound variable', () {
        final env = emptyEnv();
        final (error, updatedEnv) = updateVar('name', IrInteger(42), env);
        expect(error, isA<RuntimeException>());
        expect(error?.symbol, equals('cannot-set-unbound-variable'));
        expect(updatedEnv, isNull);
      });
    });

    group('Safety lookup', () {
      test('lookupLocal: returns Nothing on the empty stack', () {
        expect(lookupLocal('x', emptyEnv()), isNull);
      });

      test('lookupVar: returns error on empty stack', () {
        final (error, value) = lookupVar('name', emptyEnv());
        expect(error, isA<RuntimeException>());
        expect(error?.symbol, equals('unbound-variable'));
        expect(value, isNull);
      });
    });
  });

  group('Runtime Exceptions', () {
    test('unboundVariable', () {
      final exc = unboundVariable('testVar');
      expect(exc.symbol, equals('unbound-variable'));
      expect(exc.value, equals(IrString('testVar')));
      expect(exc.pretty(), contains('unbound-variable'));
    });

    test('canNotSetUnboundVariable', () {
      final exc = canNotSetUnboundVariable('testVar');
      expect(exc.symbol, equals('cannot-set-unbound-variable'));
      expect(exc.value, equals(IrString('testVar')));
    });

    test('notCallableObject', () {
      final exc = notCallableObject();
      expect(exc.symbol, equals('not-callable-object'));
      expect(exc.value, isNull);
    });

    test('wrongArgumentType', () {
      final exc = wrongArgumentType(['int', 'string']);
      expect(exc.symbol, equals('wrong-argument-type'));
      expect(exc.value, isA<IrList>());
      final list = exc.value as IrList;
      expect(list.elements.length, equals(2));
    });

    test('propertyNotFound', () {
      final exc = propertyNotFound('missingProp');
      expect(exc.symbol, equals('property-not-found'));
      expect(exc.value, equals(IrString('missingProp')));
    });

    test('notAnObject', () {
      final value = IrInteger(42);
      final exc = notAnObject(value);
      expect(exc.symbol, equals('not-an-object'));
      expect(exc.value, equals(value));
    });

    test('moduleNotFound', () {
      final exc = moduleNotFound('missingModule');
      expect(exc.symbol, equals('module-not-found'));
      expect(exc.value, equals(IrString('missingModule')));
    });

    test('runtimeException', () {
      final value = IrString('details');
      final exc = runtimeException('custom-error', value);
      expect(exc.symbol, equals('custom-error'));
      expect(exc.value, equals(value));
    });
  });
}
