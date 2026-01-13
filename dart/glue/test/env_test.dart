import 'package:glue/glue.dart' as glue;
import 'package:test/test.dart';

void main() {
  group('Glue.Env (Test stack memory model)', () {
    group('Smart constructors and atomic operations', () {
      test('emptyEnv: create empty stack environment', () {
        final env = glue.emptyEnv();
        expect(glue.lookupLocal('any', env), isNull);
        final (error, value) = glue.lookupVar('any', env);
        expect(error, isA<glue.RuntimeException>());
        expect(error?.symbol, equals('unbound-variable'));
        expect(value, isNull);
      });

      test('fromList: initialize environment stack', () {
        final env = glue.fromList([
          ('a', glue.IrInteger(1)),
          ('b', glue.IrInteger(2)),
        ]);
        expect(glue.lookupLocal('a', env), equals(glue.IrInteger(1)));
        final (error, value) = glue.lookupVar('b', env);
        expect(error, isNull);
        expect(value, equals(glue.IrInteger(2)));
      });

      test('pushFrame / popFrame: manage stack (LIFO)', () {
        final base = glue.fromList([('x', glue.IrInteger(1))]);
        final pushed = glue.pushFrame(base);
        expect(glue.lookupLocal('x', pushed), isNull);
        final (error, value) = glue.lookupVar('x', pushed);
        expect(error, isNull);
        expect(value, equals(glue.IrInteger(1)));
        expect(glue.popFrame(pushed), equals(base));
      });

      test('popFrame don\'t crash on empty stack', () {
        expect(glue.popFrame(glue.emptyEnv()), equals(glue.emptyEnv()));
      });
    });

    group('Define and search (shadowing)', () {
      test('defineVar: always define at the top frame', () {
        final env = glue.pushFrame(
          glue.fromList([('other', glue.IrInteger(0))]),
        );
        final newEnv = glue.defineVar('name', glue.IrInteger(42), env);
        expect(glue.lookupLocal('name', newEnv), equals(glue.IrInteger(42)));
      });

      test('Shadowing: local definition shadow global', () {
        final env = glue.fromList([('name', glue.IrInteger(1))]);
        final finalEnv = glue.defineVar(
          'name',
          glue.IrInteger(2),
          glue.pushFrame(env),
        );
        final (error, value) = glue.lookupVar('name', finalEnv);
        expect(error, isNull);
        expect(value, equals(glue.IrInteger(2)));
        final (error2, value2) = glue.lookupVar(
          'name',
          glue.popFrame(finalEnv),
        );
        expect(error2, isNull);
        expect(value2, equals(glue.IrInteger(1)));
      });
    });

    group('Variable updating', () {
      test(
        'updateVar: update values in the place, don\'t create a new one',
        () {
          final env = glue.pushFrame(
            glue.fromList([('x', glue.IrInteger(10))]),
          );
          final (error, updatedEnv) = glue.updateVar(
            'x',
            glue.IrInteger(20),
            env,
          );
          expect(error, isNull);
          expect(updatedEnv, isNotNull);

          expect(glue.lookupLocal('x', updatedEnv!), isNull);
          final (error2, value) = glue.lookupVar('x', updatedEnv);
          expect(error2, isNull);
          expect(value, equals(glue.IrInteger(20)));
        },
      );

      test('updateVar: return error for unbound variable', () {
        final env = glue.emptyEnv();
        final (error, updatedEnv) = glue.updateVar(
          'name',
          glue.IrInteger(42),
          env,
        );
        expect(error, isA<glue.RuntimeException>());
        expect(error?.symbol, equals('cannot-set-unbound-variable'));
        expect(updatedEnv, isNull);
      });
    });

    group('Safety lookup', () {
      test('lookupLocal: returns Nothing on the empty stack', () {
        expect(glue.lookupLocal('x', glue.emptyEnv()), isNull);
      });

      test('lookupVar: returns error on empty stack', () {
        final (error, value) = glue.lookupVar('name', glue.emptyEnv());
        expect(error, isA<glue.RuntimeException>());
        expect(error?.symbol, equals('unbound-variable'));
        expect(value, isNull);
      });
    });
  });

  group('Runtime Exceptions', () {
    test('unboundVariable', () {
      final exc = glue.unboundVariable('testVar');
      expect(exc.symbol, equals('unbound-variable'));
      expect(exc.value, equals(glue.IrString('testVar')));
      expect(exc.pretty(), contains('unbound-variable'));
    });

    test('canNotSetUnboundVariable', () {
      final exc = glue.canNotSetUnboundVariable('testVar');
      expect(exc.symbol, equals('cannot-set-unbound-variable'));
      expect(exc.value, equals(glue.IrString('testVar')));
    });

    test('notCallableObject', () {
      final exc = glue.notCallableObject();
      expect(exc.symbol, equals('not-callable-object'));
      expect(exc.value, isNull);
    });

    test('wrongArgumentType', () {
      final exc = glue.wrongArgumentType(['int', 'string']);
      expect(exc.symbol, equals('wrong-argument-type'));
      expect(exc.value, isA<glue.IrList>());
      final list = exc.value as glue.IrList;
      expect(list.elements.length, equals(2));
    });

    test('propertyNotFound', () {
      final exc = glue.propertyNotFound('missingProp');
      expect(exc.symbol, equals('property-not-found'));
      expect(exc.value, equals(glue.IrString('missingProp')));
    });

    test('notAnObject', () {
      final value = glue.IrInteger(42);
      final exc = glue.notAnObject(value);
      expect(exc.symbol, equals('not-an-object'));
      expect(exc.value, equals(value));
    });

    test('moduleNotFound', () {
      final exc = glue.moduleNotFound('missingModule');
      expect(exc.symbol, equals('module-not-found'));
      expect(exc.value, equals(glue.IrString('missingModule')));
    });

    test('runtimeException', () {
      final value = glue.IrString('details');
      final exc = glue.runtimeException('custom-error', value);
      expect(exc.symbol, equals('custom-error'));
      expect(exc.value, equals(value));
    });
  });
}
