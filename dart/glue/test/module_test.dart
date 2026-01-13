import 'package:glue/glue.dart';
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
      expect(lookupModule('test.math', newRegistry!), equals(module));
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
}
