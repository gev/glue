import 'package:glue/env.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue/src/ast.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/runtime.dart';
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
    group('Property modification with validation', () {
      test('sets string property successfully', () async {
        // Create a mutable person with a name setter
        final person = MutablePerson('Alice', 30);
        final nameGetter = Eval(
          (runtime) => Right((IrString(person.name), runtime)),
        );
        final nameSetter = (Ir value) => switch (value) {
          IrString(value: final newName) => Eval<Ir>((runtime) {
            person.name = newName;
            return Right((IrVoid(), runtime));
          }),
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('string')),
          ),
        };
        final getters = <String, Eval<Ir>>{'name': nameGetter};
        final setters = <String, Eval<Ir> Function(Ir)>{'name': nameSetter};
        final hostVal = hostValueWithProps(person, getters, setters);
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('person', hostIr, emptyEnv());

        // Set the name
        final setIr = IrList([
          IrSpecial((List<Ir> args) {
            if (args.length != 2) {
              return throwError(
                RuntimeException('wrong-number-of-arguments', IrString('2')),
              );
            }
            final target = args[0];
            final value = args[1];

            return switch (target) {
              IrDottedSymbol(parts: final parts) => _evalSetDotted(
                parts,
                value,
              ),
              IrSymbol(value: final name) => eval(value).flatMap(
                (evaluatedValue) => updateVarEval(
                  name,
                  evaluatedValue,
                ).map((_) => evaluatedValue),
              ),
              _ => throwError(
                RuntimeException(
                  'invalid-set-target',
                  IrString('symbol or dotted symbol'),
                ),
              ),
            };
          }),
          IrDottedSymbol(['person', 'name']),
          IrString('Bob'),
        ]);

        final setResult = await runEvalSimple(eval(setIr), env);
        setResult.match(
          (error) => fail('Setting property should succeed: $error'),
          (value) {
            final (result, _) = value;
            expect(result, isA<IrString>());
            expect((result as IrString).value, equals('Bob'));
          },
        );

        // Verify the change persisted
        final getIr = IrDottedSymbol(['person', 'name']);
        final getResult = await runEvalSimple(eval(getIr), env);
        getResult.match(
          (error) => fail('Getting property should succeed: $error'),
          (value) {
            final (result, _) = value;
            expect(result, equals(IrString('Bob')));
          },
        );
      });

      test('sets integer property successfully', () async {
        // Create a mutable person with an age setter
        final person = MutablePerson('Charlie', 25);
        final ageGetter = Eval(
          (runtime) => Right((IrInteger(person.age), runtime)),
        );
        final ageSetter = (Ir value) => switch (value) {
          IrInteger(value: final newAge) => Eval<Ir>((runtime) {
            person.age = newAge;
            return Right((IrVoid(), runtime));
          }),
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('integer')),
          ),
        };
        final getters = <String, Eval<Ir>>{'age': ageGetter};
        final setters = <String, Eval<Ir> Function(Ir)>{'age': ageSetter};
        final hostVal = hostValueWithProps(person, getters, setters);
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('person', hostIr, emptyEnv());

        // Set the age
        final setIr = IrList([
          IrSpecial((List<Ir> args) {
            if (args.length != 2) {
              return throwError(
                RuntimeException('wrong-number-of-arguments', IrString('2')),
              );
            }
            final target = args[0];
            final value = args[1];

            return switch (target) {
              IrDottedSymbol(parts: final parts) => _evalSetDotted(
                parts,
                value,
              ),
              IrSymbol(value: final name) => eval(value).flatMap(
                (evaluatedValue) => updateVarEval(
                  name,
                  evaluatedValue,
                ).map((_) => evaluatedValue),
              ),
              _ => throwError(
                RuntimeException(
                  'invalid-set-target',
                  IrString('symbol or dotted symbol'),
                ),
              ),
            };
          }),
          IrDottedSymbol(['person', 'age']),
          IrInteger(35),
        ]);

        final setResult = await runEvalSimple(eval(setIr), env);
        setResult.match(
          (error) => fail('Setting property should succeed: $error'),
          (value) {
            final (result, _) = value;
            expect(result, equals(IrInteger(35)));
          },
        );

        // Verify the change persisted
        final getIr = IrDottedSymbol(['person', 'age']);
        final getResult = await runEvalSimple(eval(getIr), env);
        getResult.match(
          (error) => fail('Getting property should succeed: $error'),
          (value) {
            final (result, _) = value;
            expect(result, equals(IrInteger(35)));
          },
        );
      });
    });

    group('Type validation in setters', () {
      test('rejects wrong type for string property', () async {
        final config = MutableConfig('debug', true);
        final settingGetter = Eval(
          (runtime) => Right((IrString(config.setting), runtime)),
        );
        final settingSetter = (Ir value) => switch (value) {
          IrString(value: final newSetting) => Eval<Ir>((runtime) {
            config.setting = newSetting;
            return Right((IrVoid(), runtime));
          }),
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('string')),
          ),
        };
        final getters = <String, Eval<Ir>>{'setting': settingGetter};
        final setters = <String, Eval<Ir> Function(Ir)>{
          'setting': settingSetter,
        };
        final hostVal = hostValueWithProps(config, getters, setters);
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('config', hostIr, emptyEnv());

        // Try to set with wrong type (integer instead of string)
        final setIr = IrList([
          IrSpecial((List<Ir> args) {
            if (args.length != 2) {
              return throwError(
                RuntimeException('wrong-number-of-arguments', IrString('2')),
              );
            }
            final target = args[0];
            final value = args[1];

            return switch (target) {
              IrDottedSymbol(parts: final parts) => _evalSetDotted(
                parts,
                value,
              ),
              IrSymbol(value: final name) => eval(value).flatMap(
                (evaluatedValue) => updateVarEval(
                  name,
                  evaluatedValue,
                ).map((_) => evaluatedValue),
              ),
              _ => throwError(
                RuntimeException(
                  'invalid-set-target',
                  IrString('symbol or dotted symbol'),
                ),
              ),
            };
          }),
          IrDottedSymbol(['config', 'setting']),
          IrInteger(123), // Wrong type
        ]);

        final result = await runEvalSimple(eval(setIr), env);
        result.match(
          (error) => expect(true, isTrue), // Should fail with type error
          (value) =>
              fail('Setting with wrong type should fail, but got: ${value.$1}'),
        );
      });

      test('rejects wrong type for boolean property', () async {
        final config = MutableConfig('test', false);
        final enabledGetter = Eval(
          (runtime) => Right((IrBool(config.enabled), runtime)),
        );
        final enabledSetter = (Ir value) => switch (value) {
          IrBool(value: final newEnabled) => Eval<Ir>((runtime) {
            config.enabled = newEnabled;
            return Right((IrVoid(), runtime));
          }),
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('boolean')),
          ),
        };
        final getters = <String, Eval<Ir>>{'enabled': enabledGetter};
        final setters = <String, Eval<Ir> Function(Ir)>{
          'enabled': enabledSetter,
        };
        final hostVal = hostValueWithProps(config, getters, setters);
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('config', hostIr, emptyEnv());

        // Try to set with wrong type (string instead of boolean)
        final setIr = IrList([
          IrSpecial((List<Ir> args) {
            if (args.length != 2) {
              return throwError(
                RuntimeException('wrong-number-of-arguments', IrString('2')),
              );
            }
            final target = args[0];
            final value = args[1];

            return switch (target) {
              IrDottedSymbol(parts: final parts) => _evalSetDotted(
                parts,
                value,
              ),
              IrSymbol(value: final name) => eval(value).flatMap(
                (evaluatedValue) => updateVarEval(
                  name,
                  evaluatedValue,
                ).map((_) => evaluatedValue),
              ),
              _ => throwError(
                RuntimeException(
                  'invalid-set-target',
                  IrString('symbol or dotted symbol'),
                ),
              ),
            };
          }),
          IrDottedSymbol(['config', 'enabled']),
          IrString('true'), // Wrong type
        ]);

        final result = await runEvalSimple(eval(setIr), env);
        result.match(
          (error) => expect(true, isTrue), // Should fail with type error
          (value) =>
              fail('Setting with wrong type should fail, but got: ${value.$1}'),
        );
      });
    });

    group('Multiple properties with setters', () {
      test('sets multiple properties on same object', () async {
        final person = MutablePerson('David', 40);
        final nameGetter = Eval(
          (runtime) => Right((IrString(person.name), runtime)),
        );
        final ageGetter = Eval(
          (runtime) => Right((IrInteger(person.age), runtime)),
        );
        final nameSetter = (Ir value) => switch (value) {
          IrString(value: final newName) => Eval<Ir>((runtime) {
            person.name = newName;
            return Right((IrVoid(), runtime));
          }),
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('string')),
          ),
        };
        final ageSetter = (Ir value) => switch (value) {
          IrInteger(value: final newAge) => Eval<Ir>((runtime) {
            person.age = newAge;
            return Right((IrVoid(), runtime));
          }),
          _ => throwError(
            RuntimeException('wrong-argument-type', IrString('integer')),
          ),
        };
        final getters = <String, Eval<Ir>>{
          'name': nameGetter,
          'age': ageGetter,
        };
        final setters = <String, Eval<Ir> Function(Ir)>{
          'name': nameSetter,
          'age': ageSetter,
        };
        final hostVal = hostValueWithProps(person, getters, setters);
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('person', hostIr, emptyEnv());

        // Set name
        final setNameIr = IrList([
          IrSpecial((List<Ir> args) {
            if (args.length != 2) {
              return throwError(
                RuntimeException('wrong-number-of-arguments', IrString('2')),
              );
            }
            final target = args[0];
            final value = args[1];

            return switch (target) {
              IrDottedSymbol(parts: final parts) => _evalSetDotted(
                parts,
                value,
              ),
              IrSymbol(value: final name) => eval(value).flatMap(
                (evaluatedValue) => updateVarEval(
                  name,
                  evaluatedValue,
                ).map((_) => evaluatedValue),
              ),
              _ => throwError(
                RuntimeException(
                  'invalid-set-target',
                  IrString('symbol or dotted symbol'),
                ),
              ),
            };
          }),
          IrDottedSymbol(['person', 'name']),
          IrString('Eve'),
        ]);

        await runEvalSimple(eval(setNameIr), env);

        // Set age
        final setAgeIr = IrList([
          IrSpecial((List<Ir> args) {
            if (args.length != 2) {
              return throwError(
                RuntimeException('wrong-number-of-arguments', IrString('2')),
              );
            }
            final target = args[0];
            final value = args[1];

            return switch (target) {
              IrDottedSymbol(parts: final parts) => _evalSetDotted(
                parts,
                value,
              ),
              IrSymbol(value: final name) => eval(value).flatMap(
                (evaluatedValue) => updateVarEval(
                  name,
                  evaluatedValue,
                ).map((_) => evaluatedValue),
              ),
              _ => throwError(
                RuntimeException(
                  'invalid-set-target',
                  IrString('symbol or dotted symbol'),
                ),
              ),
            };
          }),
          IrDottedSymbol(['person', 'age']),
          IrInteger(28),
        ]);

        await runEvalSimple(eval(setAgeIr), env);

        // Verify both changes
        final getNameIr = IrDottedSymbol(['person', 'name']);
        final nameResult = await runEvalSimple(eval(getNameIr), env);
        nameResult.match(
          (error) => fail('Getting name should succeed: $error'),
          (value) {
            final (result, _) = value;
            expect(result, equals(IrString('Eve')));
          },
        );

        final getAgeIr = IrDottedSymbol(['person', 'age']);
        final ageResult = await runEvalSimple(eval(getAgeIr), env);
        ageResult.match((error) => fail('Getting age should succeed: $error'), (
          value,
        ) {
          final (result, _) = value;
          expect(result, equals(IrInteger(28)));
        });
      });
    });

    group('Error handling', () {
      test('fails when setting non-existent property', () async {
        final person = MutablePerson('Frank', 50);
        final getters = <String, Eval<Ir>>{
          'name': Eval((runtime) => Right((IrString(person.name), runtime))),
        };
        final setters = <String, Eval<Ir> Function(Ir)>{};
        final hostVal = hostValueWithProps(person, getters, setters);
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('person', hostIr, emptyEnv());

        final setIr = IrList([
          IrSpecial((List<Ir> args) {
            if (args.length != 2) {
              return throwError(
                RuntimeException('wrong-number-of-arguments', IrString('2')),
              );
            }
            final target = args[0];
            final value = args[1];

            return switch (target) {
              IrDottedSymbol(parts: final parts) => _evalSetDotted(
                parts,
                value,
              ),
              IrSymbol(value: final name) => eval(value).flatMap(
                (evaluatedValue) => updateVarEval(
                  name,
                  evaluatedValue,
                ).map((_) => evaluatedValue),
              ),
              _ => throwError(
                RuntimeException(
                  'invalid-set-target',
                  IrString('symbol or dotted symbol'),
                ),
              ),
            };
          }),
          IrDottedSymbol(['person', 'nonexistent']),
          IrString('value'),
        ]);

        final result = await runEvalSimple(eval(setIr), env);
        result.match(
          (error) => expect(true, isTrue), // Should fail
          (value) => fail(
            'Setting non-existent property should fail, but got: ${value.$1}',
          ),
        );
      });

      test('fails when setting property on non-host value', () async {
        final env = defineVar('number', IrInteger(42), emptyEnv());

        final setIr = IrList([
          IrSpecial((List<Ir> args) {
            if (args.length != 2) {
              return throwError(
                RuntimeException('wrong-number-of-arguments', IrString('2')),
              );
            }
            final target = args[0];
            final value = args[1];

            return switch (target) {
              IrDottedSymbol(parts: final parts) => _evalSetDotted(
                parts,
                value,
              ),
              IrSymbol(value: final name) => eval(value).flatMap(
                (evaluatedValue) => updateVarEval(
                  name,
                  evaluatedValue,
                ).map((_) => evaluatedValue),
              ),
              _ => throwError(
                RuntimeException(
                  'invalid-set-target',
                  IrString('symbol or dotted symbol'),
                ),
              ),
            };
          }),
          IrDottedSymbol(['number', 'property']),
          IrString('value'),
        ]);

        final result = await runEvalSimple(eval(setIr), env);
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

// Helper function for testing (copied from eval.dart)
Eval<Ir> _evalSetDotted(List<String> parts, Ir value) {
  if (parts.length < 2) {
    return throwError(
      RuntimeException(
        'invalid-set-target',
        IrString('dotted symbol with property'),
      ),
    );
  }

  final baseName = parts[0];
  final prop = parts[1];

  return getEnv().flatMap((env) {
    final result = lookupVar(baseName, env);
    return result.match(
      (error) => throwError(error),
      (baseValue) => switch (baseValue) {
        IrNativeValue(value: final hostValue) =>
          hostValue.setters[prop] != null
              ? eval(value).flatMap(
                  (evaluatedValue) => hostValue.setters[prop]!(evaluatedValue),
                )
              : throwError(
                  RuntimeException('property-not-found', IrString(prop)),
                ),
        _ => throwError(
          RuntimeException('not-an-object', IrString('host value')),
        ),
      },
    );
  });
}
