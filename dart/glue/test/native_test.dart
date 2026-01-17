import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/env.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue/src/ast.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/parser.dart';
import 'package:test/test.dart';

// Test data types for host objects with mutable state
class Person {
  final String name;
  final int age;
  final Address? address;

  const Person(this.name, this.age, this.address);

  @override
  String toString() => 'Person($name, $age, $address)';

  @override
  bool operator ==(Object other) =>
      other is Person &&
      other.name == name &&
      other.age == age &&
      other.address == address;

  @override
  int get hashCode => Object.hash(name, age, address);
}

class Address {
  final String street;
  final String city;

  const Address(this.street, this.city);

  @override
  String toString() => 'Address($street, $city)';

  @override
  bool operator ==(Object other) =>
      other is Address && other.street == street && other.city == city;

  @override
  int get hashCode => Object.hash(street, city);
}

// Constructor functions that take object literals and create native objects
Eval<Ir> person(List<Ir> args) {
  return switch (args) {
    [IrObject(properties: final props)] => _createPerson(props),
    _ => throwError(
      RuntimeException('wrong-argument-type', IrString('object')),
    ),
  };
}

Eval<Ir> address(List<Ir> args) {
  return switch (args) {
    [IrObject(properties: final props)] => _createAddress(props),
    _ => throwError(
      RuntimeException('wrong-argument-type', IrString('object')),
    ),
  };
}

Eval<Ir> _createPerson(IMap<String, Ir> props) {
  final propsMap = props.unlock;
  // Extract properties from object literal with type checking
  final name = switch (props['name']) {
    IrString(value: final n) => n,
    IrString() => throw RuntimeException(
      'wrong-argument-type',
      IrString('string'),
    ),
    null => throw RuntimeException(
      'wrong-argument-type',
      IrString('name: string'),
    ),
    _ => throw RuntimeException('wrong-argument-type', IrString('string')),
  };

  final age = switch (props['age']) {
    IrInteger(value: final a) => a,
    IrInteger() => throw RuntimeException(
      'wrong-argument-type',
      IrString('integer'),
    ),
    null => throw RuntimeException(
      'wrong-argument-type',
      IrString('age: integer'),
    ),
    _ => throw RuntimeException('wrong-argument-type', IrString('integer')),
  };

  final address = switch (props['address']) {
    IrNativeValue(value: final addrHostValue) =>
      switch (extractHostValue<Address>(addrHostValue)) {
        final addr? => addr,
        null => throw RuntimeException(
          'wrong-argument-type',
          IrString('address: Address'),
        ),
      },
    IrNativeValue() => throw RuntimeException(
      'wrong-argument-type',
      IrString('NativeValue'),
    ),
    null => null,
    _ => throw RuntimeException('wrong-argument-type', IrString('NativeValue')),
  };

  final personObj = Person(name, age, address);

  // Create getters and setters
  final getters = <String, Eval<Ir>>{
    'name': Eval.pure(IrString(personObj.name)),
    'age': Eval.pure(IrInteger(personObj.age)),
    'address': switch (personObj.address) {
      final addr? => Eval.pure(IrNativeValue(hostValue(addr))),
      null => Eval.pure(IrString('no address')),
    },
  };

  final setters = <String, Eval<Ir> Function(Ir)>{
    'name': (Ir value) => switch (value) {
      IrString(value: final newName) => Eval.pure(IrVoid()),
      _ => throwError(
        RuntimeException('wrong-argument-type', IrString('string')),
      ),
    },
    'age': (Ir value) => switch (value) {
      IrInteger(value: final newAge) => Eval.pure(IrVoid()),
      _ => throwError(
        RuntimeException('wrong-argument-type', IrString('integer')),
      ),
    },
  };

  return Eval.pure(
    IrNativeValue(hostValueWithProps(personObj, getters, setters)),
  );
}

Eval<Ir> _createAddress(IMap<String, Ir> props) {
  final propsMap = props.unlock;
  // Extract properties from object literal with type checking
  final street = switch (props['street']) {
    IrString(value: final s) => s,
    IrString() => throw RuntimeException(
      'wrong-argument-type',
      IrString('string'),
    ),
    null => throw RuntimeException(
      'wrong-argument-type',
      IrString('street: string'),
    ),
    _ => throw RuntimeException('wrong-argument-type', IrString('string')),
  };

  final city = switch (props['city']) {
    IrString(value: final c) => c,
    IrString() => throw RuntimeException(
      'wrong-argument-type',
      IrString('string'),
    ),
    null => throw RuntimeException(
      'wrong-argument-type',
      IrString('city: string'),
    ),
    _ => throw RuntimeException('wrong-argument-type', IrString('string')),
  };

  final addrObj = Address(street, city);

  // Create getters and setters
  final getters = <String, Eval<Ir>>{
    'street': Eval.pure(IrString(addrObj.street)),
    'city': Eval.pure(IrString(addrObj.city)),
  };

  final setters = <String, Eval<Ir> Function(Ir)>{
    'street': (Ir value) => switch (value) {
      IrString(value: final newStreet) => Eval.pure(IrVoid()),
      _ => throwError(
        RuntimeException('wrong-argument-type', IrString('string')),
      ),
    },
    'city': (Ir value) => switch (value) {
      IrString(value: final newCity) => Eval.pure(IrVoid()),
      _ => throwError(
        RuntimeException('wrong-argument-type', IrString('string')),
      ),
    },
  };

  return Eval.pure(
    IrNativeValue(hostValueWithProps(addrObj, getters, setters)),
  );
}

// Test environment with constructors
Env testEnv() {
  return defineVar(
    'person',
    IrNativeFunc(person),
    defineVar(
      'address',
      IrNativeFunc(address),
      defineVar(
        'def',
        IrSpecial((List<Ir> args) {
          if (args.length != 2) {
            return throwError(
              RuntimeException('wrong-number-of-arguments', IrString('2')),
            );
          }
          final name = switch (args[0]) {
            IrSymbol(value: final n) => n,
            _ => throw RuntimeException(
              'wrong-argument-type',
              IrString('symbol'),
            ),
          };
          return eval(
            args[1],
          ).flatMap((value) => defineVarEval(name, value).map((_) => value));
        }),
        defineVar(
          'set',
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
          emptyEnv(),
        ),
      ),
    ),
  );
}

// Helper to handle set on dotted symbols (obj.prop = value)
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

// Helper to run Glue code
Future<Either<EvalError, Ir>> runGlueCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match(
    (parseError) {
      return Left(
        EvalError(
          [],
          RuntimeException('parse-error', IrString(parseError.message)),
        ),
      );
    },
    (ast) async {
      final ir = compile(ast);
      final result = await runEvalSimple(eval(ir), testEnv());
      return result.match(
        (error) => Left<EvalError, Ir>(error),
        (value) => Right<EvalError, Ir>(value.$1),
      );
    },
  );
}

void main() {
  group('Full FFI Integration Tests', () {
    group('Basic Object Creation and Property Access', () {
      test('creates person and accesses properties', () async {
        final result = await runGlueCode('''
(def bob (person :name "Bob" :age 25))
bob.name
''');
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Bob')));
        });
      });

      test('creates address and accesses properties', () async {
        final result = await runGlueCode('''
(def addr (address :street "123 Main St" :city "Springfield"))
addr.street
''');
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('123 Main St')));
        });
      });
    });

    group('Property Modification', () {
      test('modifies person properties', () async {
        final result = await runGlueCode('''
(def bob (person :name "Bob" :age 25))
(set bob.age 26)
bob.age
''');
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrInteger(26)));
        });
      });

      test('modifies address properties', () async {
        final result = await runGlueCode('''
(def addr (address :street "123 Main St" :city "Springfield"))
(set addr.city "Boston")
addr.city
''');
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Boston')));
        });
      });
    });

    group('Complex Object Relationships', () {
      test('creates person with address', () async {
        final result = await runGlueCode('''
(def addr (address :street "123 Main St" :city "Springfield"))
(def bob (person :name "Bob" :age 25 :address addr))
bob.address.city
''');
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Springfield')));
        });
      });

      test('modifies nested properties', () async {
        final result = await runGlueCode('''
(def addr (address :street "123 Main St" :city "Springfield"))
(def bob (person :name "Bob" :age 25 :address addr))
(set bob.address.city "Boston")
bob.address.city
''');
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Boston')));
        });
      });
    });

    group('Multiple Operations in Sequence', () {
      test('performs complex object manipulation', () async {
        final result = await runGlueCode('''
(def addr (address :street "123 Main St" :city "Springfield"))
(def bob (person :name "Bob" :age 25 :address addr))
(set bob.age 26)
(set bob.name "Robert")
(set bob.address.city "Boston")
(set bob.address.street "456 Oak Ave")
bob.name
''');
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Robert')));
        });
      });

      test('verifies all modifications persist', () async {
        final result = await runGlueCode('''
(def addr (address :street "123 Main St" :city "Springfield"))
(def bob (person :name "Bob" :age 25 :address addr))
(set bob.age 26)
(set bob.address.city "Boston")
bob.age
''');
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrInteger(26)));
        });
      });

      test('sets new address on person', () async {
        final result = await runGlueCode('''
(def addr1 (address :street "123 Main St" :city "Springfield"))
(def addr2 (address :street "456 Oak Ave" :city "Boston"))
(def bob (person :name "Bob" :age 25 :address addr1))
(set bob.address addr2)
bob.address.city
''');
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Boston')));
        });
      });
    });

    group('Error Handling', () {
      test('fails with wrong constructor arguments', () async {
        final result = await runGlueCode('(person "Bob")');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong name type', () async {
        final result = await runGlueCode('(person :name 123 :age 25)');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong age type', () async {
        final result = await runGlueCode('(person :name "Bob" :age "25")');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong address type', () async {
        final result = await runGlueCode(
          '(person :name "Bob" :age 25 :address "not-an-address")',
        );
        expect(result.isLeft, isTrue);
      });

      test('fails with missing name field', () async {
        final result = await runGlueCode('(person :age 25)');
        expect(result.isLeft, isTrue);
      });

      test('fails with missing age field', () async {
        final result = await runGlueCode('(person :name "Bob")');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong street type', () async {
        final result = await runGlueCode(
          '(address :street 123 :city "Springfield")',
        );
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong city type', () async {
        final result = await runGlueCode(
          '(address :street "123 Main St" :city 456)',
        );
        expect(result.isLeft, isTrue);
      });

      test('fails with missing street field', () async {
        final result = await runGlueCode('(address :city "Springfield")');
        expect(result.isLeft, isTrue);
      });

      test('fails with missing city field', () async {
        final result = await runGlueCode('(address :street "123 Main St")');
        expect(result.isLeft, isTrue);
      });

      test('fails accessing non-existent properties', () async {
        final result = await runGlueCode('''
(def bob (person :name "Bob" :age 25))
bob.nonexistent
''');
        expect(result.isLeft, isTrue);
      });

      test('fails setting wrong types', () async {
        final result = await runGlueCode('''
(def bob (person :name "Bob" :age 25))
(set bob.age "not-a-number")
''');
        expect(result.isLeft, isTrue);
      });
    });
  });
}
