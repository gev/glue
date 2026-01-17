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

// Test data types for host values with properties
class Person {
  final String name;
  final int age;

  const Person(this.name, this.age);

  @override
  bool operator ==(Object other) =>
      other is Person && other.name == name && other.age == age;

  @override
  int get hashCode => Object.hash(name, age);
}

class Calculator {
  final int baseValue;

  const Calculator(this.baseValue);

  @override
  bool operator ==(Object other) =>
      other is Calculator && other.baseValue == baseValue;

  @override
  int get hashCode => baseValue.hashCode;
}

void main() {
  group('HostValue property getters', () {
    group('Property access returning values', () {
      test('accesses simple property returning a string', () async {
        // Create a person with a name property
        final person = Person('Alice', 30);
        final nameGetter = Eval(
          (runtime) => Right((IrString('Alice'), runtime)),
        );
        final getters = <String, Eval<Ir>>{'name': nameGetter};
        final hostVal = hostValueWithProps(person, getters, {});
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('person', hostIr, emptyEnv());
        final dottedIr = IrDottedSymbol(['person', 'name']);

        final result = await runEvalSimple(eval(dottedIr), env);
        result.match(
          (error) => fail('Property access should succeed: $error'),
          (value) {
            final (evaluated, _) = value;
            expect(evaluated, equals(IrString('Alice')));
          },
        );
      });

      test('accesses property returning a number', () async {
        // Create a person with an age property
        final person = Person('Bob', 25);
        final ageGetter = Eval((runtime) => Right((IrInteger(25), runtime)));
        final getters = <String, Eval<Ir>>{'age': ageGetter};
        final hostVal = hostValueWithProps(person, getters, {});
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('person', hostIr, emptyEnv());
        final dottedIr = IrDottedSymbol(['person', 'age']);

        final result = await runEvalSimple(eval(dottedIr), env);
        result.match(
          (error) => fail('Property access should succeed: $error'),
          (value) {
            final (evaluated, _) = value;
            expect(evaluated, equals(IrInteger(25)));
          },
        );
      });
    });

    group('Method calls returning computed values', () {
      test('calls method that computes and returns a value', () async {
        // Create a calculator with a compute method
        final calc = Calculator(10);
        final computeGetter = Eval(
          (runtime) => Right((IrInteger(42), runtime)),
        ); // Always returns 42
        final getters = <String, Eval<Ir>>{'compute': computeGetter};
        final hostVal = hostValueWithProps(calc, getters, {});
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('calc', hostIr, emptyEnv());
        final dottedIr = IrDottedSymbol(['calc', 'compute']);

        final result = await runEvalSimple(eval(dottedIr), env);
        result.match((error) => fail('Method call should succeed: $error'), (
          value,
        ) {
          final (evaluated, _) = value;
          expect(evaluated, equals(IrInteger(42)));
        });
      });

      test('calls method that accesses host object data', () async {
        // Create a calculator that returns double its base value
        final calc = Calculator(15);
        final doubleGetter = Eval(
          (runtime) => Right((IrInteger(30), runtime)),
        ); // 15 * 2
        final getters = <String, Eval<Ir>>{'double': doubleGetter};
        final hostVal = hostValueWithProps(calc, getters, {});
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('calc', hostIr, emptyEnv());
        final dottedIr = IrDottedSymbol(['calc', 'double']);

        final result = await runEvalSimple(eval(dottedIr), env);
        result.match((error) => fail('Method call should succeed: $error'), (
          value,
        ) {
          final (evaluated, _) = value;
          expect(evaluated, equals(IrInteger(30)));
        });
      });
    });

    group('Multiple properties on same object', () {
      test('accesses different properties on the same host value', () async {
        final person = Person('Charlie', 35);
        final nameGetter = Eval(
          (runtime) => Right((IrString('Charlie'), runtime)),
        );
        final ageGetter = Eval((runtime) => Right((IrInteger(35), runtime)));
        final getters = <String, Eval<Ir>>{
          'name': nameGetter,
          'age': ageGetter,
        };
        final hostVal = hostValueWithProps(person, getters, {});
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('person', hostIr, emptyEnv());

        // Test name property
        final nameDotted = IrDottedSymbol(['person', 'name']);
        final nameResult = await runEvalSimple(eval(nameDotted), env);
        nameResult.match(
          (error) => fail('Name property access should succeed: $error'),
          (value) {
            final (nameVal, _) = value;
            expect(nameVal, equals(IrString('Charlie')));
          },
        );

        // Test age property
        final ageDotted = IrDottedSymbol(['person', 'age']);
        final ageResult = await runEvalSimple(eval(ageDotted), env);
        ageResult.match(
          (error) => fail('Age property access should succeed: $error'),
          (value) {
            final (ageVal, _) = value;
            expect(ageVal, equals(IrInteger(35)));
          },
        );
      });
    });

    group('Error handling', () {
      test('fails when accessing non-existent property', () async {
        final person = Person('David', 40);
        final getters = <String, Eval<Ir>>{
          'name': Eval((runtime) => Right((IrString('David'), runtime))),
        };
        final hostVal = hostValueWithProps(person, getters, {});
        final hostIr = IrNativeValue(hostVal);
        final env = defineVar('person', hostIr, emptyEnv());
        final dottedIr = IrDottedSymbol(['person', 'nonexistent']);

        final result = await runEvalSimple(eval(dottedIr), env);
        result.match(
          (error) => expect(true, isTrue), // Should fail
          (value) => fail(
            'Accessing non-existent property should fail, but got: ${value.$1}',
          ),
        );
      });

      test('fails when accessing property on non-host value', () async {
        final env = defineVar('number', IrInteger(42), emptyEnv());
        final dottedIr = IrDottedSymbol(['number', 'property']);

        final result = await runEvalSimple(eval(dottedIr), env);
        result.match(
          (error) => expect(true, isTrue), // Should fail
          (value) => fail(
            'Accessing property on non-host value should fail, but got: ${value.$1}',
          ),
        );
      });
    });
  });
}
