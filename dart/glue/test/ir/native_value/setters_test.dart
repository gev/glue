import 'package:glue/env.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/lib/builtin/set.dart';
import 'package:test/test.dart';

// Test data types for host values with mutable properties
class MutablePerson {
  String name;
  int age;

  MutablePerson(this.name, this.age);

  @override
  bool operator ==(Object other) =>
      other is MutablePerson && other.name == name && other.age == age;

  @override
  int get hashCode => Object.hash(name, age);
}

class MutableConfig {
  String setting;
  bool enabled;

  MutableConfig(this.setting, this.enabled);

  @override
  bool operator ==(Object other) =>
      other is MutableConfig &&
      other.setting == setting &&
      other.enabled == enabled;

  @override
  int get hashCode => Object.hash(setting, enabled);
}

void main() {
  group('HostValue property setters', () {
    group('Property setting with type checking', () {
      test('sets string property with correct type', () async {
        // Create a setter that validates string type
        Eval<Ir> nameSetter(Ir newVal) => switch (newVal) {
          IrString() => Eval.pure<Ir>(IrVoid()), // Accept any string
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('string')),
          ),
        };
        final setters = <String, Eval<Ir> Function(Ir)>{
          'name': (Ir value) => nameSetter(value),
        };
        final hostVal = hostValueWithProps((), {}, setters);
        final hostIr = IrNativeValue(hostVal);

        // Set up environment and call set function
        final env = defineVar('obj', hostIr, emptyEnv());
        final setArgs = [IrSymbol('obj.name'), IrString('Alice')];

        final result = await runEvalSimple(set(setArgs), env);
        result.match(
          (error) => fail('Property setting should succeed: $error'),
          (value) {
            final (result, _) = value;
            expect(result, equals(IrVoid())); // Success
          },
        );
      });

      test('sets numeric property with correct type', () async {
        // Create a setter that validates integer type
        Eval<Ir> sizeSetter(Ir newVal) => switch (newVal) {
          IrInteger() => Eval.pure<Ir>(IrVoid()), // Accept any integer
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('integer')),
          ),
        };
        final setters = <String, Eval<Ir> Function(Ir)>{'size': sizeSetter};
        final hostVal = hostValueWithProps((), {}, setters);
        final hostIr = IrNativeValue(hostVal);

        // Set up environment and call set function
        final env = defineVar('obj', hostIr, emptyEnv());
        final setArgs = [IrSymbol('obj.size'), IrInteger(100)];

        final result = await runEvalSimple(set(setArgs), env);
        result.match(
          (error) => fail('Property setting should succeed: $error'),
          (value) {
            final (result, _) = value;
            expect(result, equals(IrVoid())); // Success
          },
        );
      });
    });

    group('Type checking and error handling', () {
      test('throws error when setting string property to wrong type', () async {
        // Create a setter that only accepts strings
        Eval<Ir> nameSetter(Ir newVal) => switch (newVal) {
          IrString() => Eval.pure<Ir>(IrVoid()),
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('string')),
          ),
        };
        final setters = <String, Eval<Ir> Function(Ir)>{'name': nameSetter};
        final hostVal = hostValueWithProps((), {}, setters);
        final hostIr = IrNativeValue(hostVal);

        // Try to set name to an integer (wrong type)
        final env = defineVar('obj', hostIr, emptyEnv());
        final setArgs = [IrSymbol('obj.name'), IrInteger(123)];

        final result = await runEvalSimple(set(setArgs), env);
        result.match(
          (error) => expect(true, isTrue), // Should fail with type error
          (value) => fail(
            'Setting string property to integer should fail, but got: ${value.$1}',
          ),
        );
      });

      test('throws error when setting numeric property to wrong type', () async {
        // Create a setter that only accepts integers
        Eval<Ir> ageSetter(Ir newVal) => switch (newVal) {
          IrInteger() => Eval.pure<Ir>(IrVoid()),
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('integer')),
          ),
        };
        final setters = <String, Eval<Ir> Function(Ir)>{'age': ageSetter};
        final hostVal = hostValueWithProps((), {}, setters);
        final hostIr = IrNativeValue(hostVal);

        // Try to set age to a string (wrong type)
        final env = defineVar('obj', hostIr, emptyEnv());
        final setArgs = [IrSymbol('obj.age'), IrString('thirty')];

        final result = await runEvalSimple(set(setArgs), env);
        result.match(
          (error) => expect(true, isTrue), // Should fail with type error
          (value) => fail(
            'Setting numeric property to string should fail, but got: ${value.$1}',
          ),
        );
      });
    });

    group('Multiple properties with different types', () {
      test('sets different typed properties on the same object', () async {
        Eval<Ir> nameSetter(Ir newVal) => switch (newVal) {
          IrString() => Eval.pure<Ir>(IrVoid()),
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('string')),
          ),
        };
        Eval<Ir> ageSetter(Ir newVal) => switch (newVal) {
          IrInteger() => Eval.pure<Ir>(IrVoid()),
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('integer')),
          ),
        };
        final setters = <String, Eval<Ir> Function(Ir)>{
          'name': nameSetter,
          'age': ageSetter,
        };
        final hostVal = hostValueWithProps((), {}, setters);
        final hostIr = IrNativeValue(hostVal);

        final env = defineVar('obj', hostIr, emptyEnv());

        // Set name (string)
        final setNameArgs = [IrSymbol('obj.name'), IrString('Bob')];
        final result1 = await runEvalSimple(set(setNameArgs), env);
        result1.match((error) => fail('Name setting should succeed: $error'), (
          value,
        ) {
          final (result, _) = value;
          expect(result, equals(IrVoid()));
        });

        // Set age (integer)
        final setAgeArgs = [IrSymbol('obj.age'), IrInteger(25)];
        final result2 = await runEvalSimple(set(setAgeArgs), env);
        result2.match((error) => fail('Age setting should succeed: $error'), (
          value,
        ) {
          final (result, _) = value;
          expect(result, equals(IrVoid()));
        });
      });
    });

    group('Error handling for missing properties', () {
      test('fails when setting non-existent property', () async {
        final hostVal = hostValueWithProps((), {}, {});
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('obj', hostIr, emptyEnv());
        final setArgs = [IrSymbol('obj.nonexistent'), IrString('value')];

        final result = await runEvalSimple(set(setArgs), env);
        result.match(
          (error) => expect(true, isTrue), // Should fail
          (value) => fail(
            'Setting non-existent property should fail, but got: ${value.$1}',
          ),
        );
      });

      test('fails when setting property on non-host value', () async {
        final env = defineVar('number', IrInteger(42), emptyEnv());
        final setArgs = [IrSymbol('number.property'), IrString('value')];

        final result = await runEvalSimple(set(setArgs), env);
        result.match(
          (error) => expect(true, isTrue), // Should fail
          (value) => fail(
            'Setting property on non-host value should fail, but got: ${value.$1}',
          ),
        );
      });
    });
  });
}
