import 'package:glue/src/either.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/ir.dart' hide Env;
import 'package:glue/src/eval/exception.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Env (Test stack memory model)', () {
    group('Smart constructors and atomic operations', () {
      test('emptyEnv: create empty stack environment', () {
        final env = emptyEnv();
        expect(env, isEmpty);
      });

      test('fromList: initialize environment stack', () {
        final env = fromList([('x', IrInteger(42)), ('y', IrString('hello'))]);
        expect(env.length, equals(1));
        expect(env[0]['x'], equals(IrInteger(42)));
        expect(env[0]['y'], equals(IrString('hello')));
      });

      test('pushFrame / popFrame: manage stack (LIFO)', () {
        var env = fromList([('x', IrInteger(42))]);
        env = pushFrame(env);
        env = defineVar('y', IrString('hello'), env);
        expect(env.length, equals(2));
        expect(env[1]['y'], equals(IrString('hello')));

        env = popFrame(env);
        expect(env.length, equals(1));
        expect(env[0]['x'], equals(IrInteger(42)));
      });

      test('popFrame don\'t crash on empty stack', () {
        final env = emptyEnv();
        final result = popFrame(env);
        expect(result, equals(env));
      });
    });

    group('Define and search (shadowing)', () {
      test('defineVar: always define at the top frame', () {
        var env = fromList([('x', IrInteger(42))]);
        env = pushFrame(env);
        env = defineVar('x', IrString('shadowed'), env);
        expect(env[1]['x'], equals(IrString('shadowed')));
        expect(env[0]['x'], equals(IrInteger(42)));
      });

      test('Shadowing: local definition shadow global', () {
        var env = fromList([('x', IrInteger(42))]);
        env = pushFrame(env);
        env = defineVar('x', IrString('shadowed'), env);

        final result = lookupVar('x', env);
        switch (result) {
          case Left(:final value):
            fail('Should not be left: $value');
          case Right(:final value):
            expect(value, equals(IrString('shadowed')));
        }
      });
    });

    group('Variable updating', () {
      test(
        'updateVar: update values in the place, don\'t create a new one',
        () {
          var env = fromList([('x', IrInteger(42))]);
          env = pushFrame(env);
          env = defineVar('y', IrString('hello'), env);

          final result = updateVar('x', IrInteger(100), env);
          switch (result) {
            case Left(:final value):
              fail('Should not be left: $value');
            case Right(:final value):
              expect(value[0]['x'], equals(IrInteger(100)));
          }
        },
      );

      test('updateVar: return error for unbound variable', () {
        final env = fromList([('x', IrInteger(42))]);

        final result = updateVar('nonexistent', IrString('value'), env);
        switch (result) {
          case Left(:final value):
            expect(value.symbol, equals('cannot-set-unbound-variable'));
          case Right(:final value):
            fail('Should not be right: $value');
        }
      });
    });

    group('Safety lookup', () {
      test('lookupLocal: returns Nothing on the empty stack', () {
        final env = emptyEnv();
        final result = lookupLocal('x', env);
        expect(result, isNull);
      });

      test('lookupVar: returns error on empty stack', () {
        final env = emptyEnv();

        final result = lookupVar('x', env);
        switch (result) {
          case Left(:final value):
            expect(value.symbol, equals('unbound-variable'));
          case Right(:final value):
            fail('Should not be right: $value');
        }
      });
    });
  });

  group('Runtime Exceptions', () {
    test('unboundVariable', () {
      final exception = unboundVariable('test');
      expect(exception.symbol, equals('unbound-variable'));
      expect(exception.value, equals(IrString('test')));
    });
  });
}
