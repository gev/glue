import 'package:glue/src/env.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/module/cache.dart';
import 'package:glue/src/module/registry.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';

void main() {
  group('Runtime System', () {
    test('Runtime.initial creates runtime with empty module system', () {
      final env = fromList([('x', IrInteger(42))]);
      final runtime = Runtime.initial(env);

      expect(runtime.env, equals(env));
      expect(runtime.context, isEmpty);
      expect(registrySize(runtime.registry), equals(0));
      expect(cacheSize(runtime.importCache), equals(0));
      expect(runtime.rootEnv, equals(env));
    });

    test('Runtime constructor creates runtime with provided values', () {
      final env = fromList([('x', IrInteger(42))]);
      final context = ['main', 'helper'];
      final registry = emptyRegistry();
      final cache = emptyCache();
      final rootEnv = fromList([('y', IrInteger(24))]);

      final runtime = Runtime(
        env: env,
        context: context,
        registry: registry,
        importCache: cache,
        rootEnv: rootEnv,
      );

      expect(runtime.env, equals(env));
      expect(runtime.context, equals(context));
      expect(runtime.registry, equals(registry));
      expect(runtime.importCache, equals(cache));
      expect(runtime.rootEnv, equals(rootEnv));
    });

    test('copyWith creates modified copy', () {
      final original = Runtime.initial(fromList([('x', IrInteger(42))]));
      final newEnv = fromList([('y', IrInteger(24))]);
      final newContext = ['test'];

      final modified = original.copyWith(env: newEnv, context: newContext);

      // Original unchanged
      expect(original.env, equals(fromList([('x', IrInteger(42))])));
      expect(original.context, isEmpty);

      // Modified has new values
      expect(modified.env, equals(newEnv));
      expect(modified.context, equals(newContext));

      // Other fields unchanged
      expect(modified.registry, equals(original.registry));
      expect(modified.importCache, equals(original.importCache));
      expect(modified.rootEnv, equals(original.rootEnv));
    });

    test('copyWith with null values keeps original', () {
      final original = Runtime.initial(fromList([('x', IrInteger(42))]));
      final modified = original.copyWith(); // No changes

      expect(modified, equals(original));
    });

    test('Runtime copyWith preserves equality for unchanged fields', () {
      final original = Runtime.initial(fromList([('x', IrInteger(42))]));
      final modified = original.copyWith(); // No changes

      // Should be equal since no changes were made
      expect(modified.env, equals(original.env));
      expect(modified.context, equals(original.context));
      expect(modified.registry, equals(original.registry));
      expect(modified.importCache, equals(original.importCache));
      expect(modified.rootEnv, equals(original.rootEnv));
    });

    test('Runtime with different env are not equal', () {
      final runtime1 = Runtime.initial(fromList([('x', IrInteger(42))]));
      final runtime2 = Runtime.initial(fromList([('y', IrInteger(24))]));

      expect(runtime1, isNot(equals(runtime2)));
    });

    test('Runtime with different context are not equal', () {
      final env = fromList([('x', IrInteger(42))]);
      final runtime1 = Runtime.initial(env);
      final runtime2 = runtime1.copyWith(context: ['test']);

      expect(runtime1, isNot(equals(runtime2)));
    });

    test('Runtime toString provides useful information', () {
      final env = fromList([('x', IrInteger(42))]);
      final runtime = Runtime.initial(env).copyWith(context: ['main', 'func']);

      final string = runtime.toString();
      expect(string, contains('env: 1 frames'));
      expect(string, contains('context: [main, func]'));
      expect(string, contains('registry: 0 modules'));
      expect(string, contains('cache: 0 imported'));
    });

    test('Runtime with module system state', () {
      final env = fromList([('x', IrInteger(42))]);
      final registry = emptyRegistry();
      final cache = emptyCache();

      // Add a module to registry
      final module = RegisteredModule(
        name: 'test.module',
        exports: ['func'],
        body: [IrInteger(1)],
      );
      final (error, registryWithModule) = registerModule(registry, module);
      expect(error, isNull);

      // Add an imported module to cache
      final imported = ImportedModule(
        moduleName: 'imported.module',
        exportedValues: {'value': IrInteger(2)},
        evaluationRootEnv: env,
      );
      final cacheWithModule = storeImportedModule(cache, imported);

      final runtime = Runtime.initial(
        env,
      ).copyWith(registry: registryWithModule!, importCache: cacheWithModule);

      expect(registrySize(runtime.registry), equals(1));
      expect(cacheSize(runtime.importCache), equals(1));

      final string = runtime.toString();
      expect(string, contains('registry: 1 modules'));
      expect(string, contains('cache: 1 imported'));
    });
  });
}
