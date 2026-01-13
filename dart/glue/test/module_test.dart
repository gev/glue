import 'package:glue/src/env.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/module_cache.dart';
import 'package:glue/src/module_registration.dart';
import 'package:glue/src/module_registry.dart';
import 'package:test/test.dart';

void main() {
  group('Module System Data Structures', () {
    test('RegisteredModule creation and equality', () {
      final module = RegisteredModule(
        name: 'test.math',
        exports: ['add', 'multiply'],
        body: [IrInteger(1), IrInteger(2)],
      );

      expect(module.name, equals('test.math'));
      expect(module.exports, equals(['add', 'multiply']));
      expect(module.body.length, equals(2));

      final module2 = RegisteredModule(
        name: 'test.math',
        exports: ['add', 'multiply'],
        body: [IrInteger(1), IrInteger(2)],
      );

      expect(module, equals(module2));
    });

    test('ImportedModule creation and equality', () {
      final env = fromList([('x', IrInteger(42))]);
      final imported = ImportedModule(
        moduleName: 'test.math',
        exportedValues: {'add': IrInteger(1), 'multiply': IrInteger(2)},
        evaluationRootEnv: env,
      );

      expect(imported.moduleName, equals('test.math'));
      expect(imported.exportedValues.length, equals(2));
      expect(imported.exportedValues['add'], equals(IrInteger(1)));

      final imported2 = ImportedModule(
        moduleName: 'test.math',
        exportedValues: {'add': IrInteger(1), 'multiply': IrInteger(2)},
        evaluationRootEnv: env,
      );

      expect(imported, equals(imported2));
    });

    test('ModuleInfo creation and equality', () {
      final definitions = [('add', IrInteger(1)), ('multiply', IrInteger(2))];
      final module = ModuleInfo(
        moduleName: 'test.math',
        exports: ['add', 'multiply'],
        definitions: definitions,
      );

      expect(module.moduleName, equals('test.math'));
      expect(module.exports, equals(['add', 'multiply']));
      expect(module.definitions.length, equals(2));

      final module2 = ModuleInfo(
        moduleName: 'test.math',
        exports: ['add', 'multiply'],
        definitions: definitions,
      );

      expect(module, equals(module2));
    });

    test('nativeModule factory', () {
      final definitions = [('add', IrInteger(1)), ('multiply', IrInteger(2))];
      final module = nativeModule('test.math', definitions);

      expect(module.moduleName, equals('test.math'));
      expect(module.exports, equals(['add', 'multiply'])); // All exported
      expect(module.definitions, equals(definitions));
    });

    test('envFromModule creates environment', () {
      final definitions = [('x', IrInteger(42)), ('y', IrInteger(24))];
      final module = ModuleInfo(
        moduleName: 'test',
        exports: ['x', 'y'],
        definitions: definitions,
      );

      final env = envFromModule(module);
      expect(env.length, equals(1)); // One frame

      final frame = env[0];
      expect(frame['x'], equals(IrInteger(42)));
      expect(frame['y'], equals(IrInteger(24)));
    });
  });

  group('Module Registry', () {
    test('emptyRegistry creates empty registry', () {
      final registry = emptyRegistry();
      expect(registrySize(registry), equals(0));
      expect(registeredModuleNames(registry), isEmpty);
    });

    test('registerModule adds module successfully', () {
      final registry = emptyRegistry();
      final module = RegisteredModule(
        name: 'test.math',
        exports: ['add'],
        body: [IrInteger(1)],
      );

      final (error, newRegistry) = registerModule(registry, module);
      expect(error, isNull);
      expect(newRegistry, isNotNull);
      expect(registrySize(newRegistry!), equals(1));
      expect(lookupModule('test.math', newRegistry), equals(module));
    });

    test('registerModule rejects duplicate names', () {
      final registry = emptyRegistry();
      final module1 = RegisteredModule(
        name: 'test.math',
        exports: ['add'],
        body: [IrInteger(1)],
      );
      final module2 = RegisteredModule(
        name: 'test.math',
        exports: ['multiply'],
        body: [IrInteger(2)],
      );

      final (error1, registry1) = registerModule(registry, module1);
      expect(error1, isNull);

      final (error2, registry2) = registerModule(registry1!, module2);
      expect(error2, isNotNull);
      expect(error2, contains('already registered'));
      expect(registry2, isNull);
    });

    test('registerModules handles multiple modules', () {
      final registry = emptyRegistry();
      final modules = [
        RegisteredModule(
          name: 'math.add',
          exports: ['add'],
          body: [IrInteger(1)],
        ),
        RegisteredModule(
          name: 'math.mul',
          exports: ['mul'],
          body: [IrInteger(2)],
        ),
      ];

      final (error, newRegistry) = registerModules(registry, modules);
      expect(error, isNull);
      expect(newRegistry, isNotNull);
      expect(registrySize(newRegistry!), equals(2));
    });
  });

  group('Module Cache', () {
    test('emptyCache creates empty cache', () {
      final cache = emptyCache();
      expect(cacheSize(cache), equals(0));
      expect(cachedModuleNames(cache), isEmpty);
    });

    test('storeImportedModule adds to cache', () {
      final cache = emptyCache();
      final env = fromList([('x', IrInteger(42))]);
      final imported = ImportedModule(
        moduleName: 'test.math',
        exportedValues: {'add': IrInteger(1)},
        evaluationRootEnv: env,
      );

      final newCache = storeImportedModule(cache, imported);
      expect(cacheSize(newCache), equals(1));
      expect(lookupImportedModule('test.math', newCache), equals(imported));
      expect(isModuleCached('test.math', newCache), isTrue);
    });

    test('removeFromCache removes module', () {
      final cache = emptyCache();
      final env = fromList([('x', IrInteger(42))]);
      final imported = ImportedModule(
        moduleName: 'test.math',
        exportedValues: {'add': IrInteger(1)},
        evaluationRootEnv: env,
      );

      final cacheWithModule = storeImportedModule(cache, imported);
      expect(cacheSize(cacheWithModule), equals(1));

      final cacheWithoutModule = removeFromCache('test.math', cacheWithModule);
      expect(cacheSize(cacheWithoutModule), equals(0));
      expect(lookupImportedModule('test.math', cacheWithoutModule), isNull);
    });

    test('clearCache empties cache', () {
      final cache = emptyCache();
      final env = fromList([('x', IrInteger(42))]);
      final imported = ImportedModule(
        moduleName: 'test.math',
        exportedValues: {'add': IrInteger(1)},
        evaluationRootEnv: env,
      );

      final cacheWithModule = storeImportedModule(cache, imported);
      expect(cacheSize(cacheWithModule), equals(1));

      final emptyCacheAgain = clearCache();
      expect(cacheSize(emptyCacheAgain), equals(0));
    });
  });

  group('Module Registration', () {
    test('parseModule: parses valid module IR', () {
      final ir = IrList([
        IrSymbol('module'),
        IrSymbol('test.math'),
        IrList([IrSymbol('export'), IrSymbol('add'), IrSymbol('multiply')]),
        IrList([IrSymbol('def'), IrSymbol('add'), IrInteger(1)]),
        IrList([IrSymbol('def'), IrSymbol('multiply'), IrInteger(2)]),
      ]);

      final (error, module) = parseModule(ir);
      expect(error, isNull);
      expect(module, isNotNull);
      expect(module!.name, equals('test.math'));
      expect(module.exports, equals(['add', 'multiply']));
      expect(module.body.length, equals(2));
    });

    test('parseModule: rejects non-list IR', () {
      final ir = IrSymbol('not-a-list');
      final (error, module) = parseModule(ir);
      expect(error, isNotNull);
      expect(error, contains('must be a non-empty list'));
      expect(module, isNull);
    });

    test('parseModule: rejects empty list', () {
      final ir = IrList([]);
      final (error, module) = parseModule(ir);
      expect(error, isNotNull);
      expect(error, contains('must be a non-empty list'));
      expect(module, isNull);
    });

    test('parseModule: rejects non-module list', () {
      final ir = IrList([IrSymbol('def'), IrSymbol('x'), IrInteger(1)]);
      final (error, module) = parseModule(ir);
      expect(error, isNotNull);
      expect(error, contains('must start with "module"'));
      expect(module, isNull);
    });

    test('parseModule: rejects invalid module name', () {
      final ir = IrList([
        IrSymbol('module'),
        IrInteger(42), // Invalid name
        IrList([IrSymbol('export'), IrSymbol('add')]),
        IrList([IrSymbol('def'), IrSymbol('add'), IrInteger(1)]),
      ]);

      final (error, module) = parseModule(ir);
      expect(error, isNotNull);
      expect(error, contains('Module name must be a symbol'));
      expect(module, isNull);
    });

    test('parseModule: handles module without body', () {
      final ir = IrList([
        IrSymbol('module'),
        IrSymbol('empty.module'),
        IrList([IrSymbol('export')]),
      ]);

      final (error, module) = parseModule(ir);
      expect(error, isNull);
      expect(module, isNotNull);
      expect(module!.name, equals('empty.module'));
      expect(module.exports, isEmpty);
      expect(module.body, isEmpty);
    });

    test('buildRegistry: builds registry from multiple modules', () {
      final modules = [
        IrList([
          IrSymbol('module'),
          IrSymbol('math.add'),
          IrList([IrSymbol('export'), IrSymbol('add')]),
          IrList([IrSymbol('def'), IrSymbol('add'), IrInteger(1)]),
        ]),
        IrList([
          IrSymbol('module'),
          IrSymbol('math.mul'),
          IrList([IrSymbol('export'), IrSymbol('multiply')]),
          IrList([IrSymbol('def'), IrSymbol('multiply'), IrInteger(2)]),
        ]),
      ];

      final (error, registry) = buildRegistry(modules);
      expect(error, isNull);
      expect(registry, isNotNull);
      expect(registrySize(registry!), equals(2));
      expect(lookupModule('math.add', registry), isNotNull);
      expect(lookupModule('math.mul', registry), isNotNull);
    });

    test('buildRegistry: handles parsing errors', () {
      final modules = [
        IrList([
          IrSymbol('module'),
          IrSymbol('valid.module'),
          IrList([IrSymbol('export'), IrSymbol('x')]),
          IrList([IrSymbol('def'), IrSymbol('x'), IrInteger(1)]),
        ]),
        IrSymbol('invalid-module'), // This will cause parsing error
      ];

      final (error, registry) = buildRegistry(modules);
      expect(error, isNotNull);
      expect(error, contains('must be a non-empty list'));
      expect(registry, isNull);
    });

    test('registerModulesFromIR: registers into existing registry', () {
      final initialRegistry = emptyRegistry();
      final (initError, registry1) = registerModule(
        initialRegistry,
        RegisteredModule(
          name: 'existing',
          exports: ['x'],
          body: [IrInteger(1)],
        ),
      );
      expect(initError, isNull);

      final modules = [
        IrList([
          IrSymbol('module'),
          IrSymbol('new.module'),
          IrList([IrSymbol('export'), IrSymbol('y')]),
          IrList([IrSymbol('def'), IrSymbol('y'), IrInteger(2)]),
        ]),
      ];

      final (error, finalRegistry) = registerModulesFromIR(registry1!, modules);
      expect(error, isNull);
      expect(finalRegistry, isNotNull);
      expect(registrySize(finalRegistry!), equals(2));
      expect(lookupModule('existing', finalRegistry), isNotNull);
      expect(lookupModule('new.module', finalRegistry), isNotNull);
    });
  });
}
