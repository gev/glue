import 'package:glue/either.dart';
import 'package:glue/src/context.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/module/cache.dart';
import 'package:glue/src/module/registry.dart';
import 'package:glue/src/ref.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';

void main() {
  group('Runtime System', () {
    test('Runtime.initial creates runtime with empty module system', () {
      final env = fromList([('x', IrInteger(42))]);
      final runtime = Runtime.initial(env);

      expect(runtime.env, equals(env));
      expect(runtime.stack, isEmpty);
      expect(registrySize(runtime.registry), equals(0));
      expect(cacheSize(runtime.importCache), equals(0));
      expect(runtime.rootEnv, equals(env));
    });

    test('Runtime constructor creates runtime with provided values', () {
      final env = fromList([('x', IrInteger(42))]);
      final stack = ['main', 'helper'];
      final registry = emptyRegistry();
      final cache = emptyCache();
      final rootEnv = fromList([('y', IrInteger(24))]);

      final runtime = Runtime(
        env: env,
        stack: stack,
        registry: registry,
        importCache: cache,
        rootEnv: rootEnv,
        context: Context.empty(),
      );

      expect(runtime.env, equals(env));
      expect(runtime.stack, equals(stack));
      expect(runtime.registry, equals(registry));
      expect(runtime.importCache, equals(cache));
      expect(runtime.rootEnv, equals(rootEnv));
    });

    test('copyWith creates modified copy', () {
      final original = Runtime.initial(fromList([('x', IrInteger(42))]));
      final newEnv = fromList([('y', IrInteger(24))]);
      final newCall = ['test'];

      final modified = original.copyWith(env: newEnv, stack: newCall);

      // Original unchanged
      expect(original.env, equals(fromList([('x', IrInteger(42))])));
      expect(original.stack, isEmpty);

      // Modified has new values
      expect(modified.env, equals(newEnv));
      expect(modified.stack, equals(newCall));

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
      expect(modified.stack, equals(original.stack));
      expect(modified.registry, equals(original.registry));
      expect(modified.importCache, equals(original.importCache));
      expect(modified.rootEnv, equals(original.rootEnv));
    });

    test('Runtime with different env are not equal', () {
      final runtime1 = Runtime.initial(fromList([('x', IrInteger(42))]));
      final runtime2 = Runtime.initial(fromList([('y', IrInteger(24))]));

      expect(runtime1, isNot(equals(runtime2)));
    });

    test('Runtime with different stack are not equal', () {
      final env = fromList([('x', IrInteger(42))]);
      final runtime1 = Runtime.initial(env);
      final runtime2 = runtime1.copyWith(stack: ['test']);

      expect(runtime1, isNot(equals(runtime2)));
    });

    test('Runtime toString provides useful information', () {
      final env = fromList([('x', IrInteger(42))]);
      final runtime = Runtime.initial(env).copyWith(stack: ['main', 'func']);

      final string = runtime.toString();
      expect(string, contains('env: 1 frames'));
      expect(string, contains('stack: [main, func]'));
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
      final (error, registryWithModule) = split(
        registerModule(registry, module),
      );
      expect(error, isNull);

      // Add an imported module to cache
      final imported = ImportedModule(
        moduleName: 'imported.module',
        exportedValues: {'value': Ref(IrInteger(2))},
        evaluationRootEnv: env,
      );
      storeImportedModule(cache, imported);

      final runtime = Runtime.initial(
        env,
      ).copyWith(registry: registryWithModule!, importCache: cache);

      expect(registrySize(runtime.registry), equals(1));
      expect(cacheSize(runtime.importCache), equals(1));

      final string = runtime.toString();
      expect(string, contains('registry: 1 modules'));
      expect(string, contains('cache: 1 imported'));
    });
  });
}
