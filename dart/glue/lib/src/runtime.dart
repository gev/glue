import 'package:glue/src/context.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/module/cache.dart';
import 'package:glue/src/module/registry.dart';

/// Runtime state for Glue evaluation
/// Mirrors Haskell Glue.Eval.Runtime exactly

/// Complete evaluation runtime containing all state
class Runtime {
  final Env env;
  final CallStack stack;
  final ModuleRegistry registry;
  final ImportedModuleCache importCache;
  final Env rootEnv;
  final Context context;

  Runtime({
    required this.env,
    required this.stack,
    required this.registry,
    required this.importCache,
    required this.rootEnv,
    required this.context,
  });

  /// Create initial runtime with empty module system
  factory Runtime.initial(Env initialEnv) => Runtime(
    env: initialEnv,
    stack: [],
    registry: emptyRegistry(),
    importCache: emptyCache(),
    rootEnv: initialEnv,
    context: Context.empty(),
  );

  /// Create a copy with modified fields
  Runtime copyWith({
    Env? env,
    CallStack? stack,
    ModuleRegistry? registry,
    ImportedModuleCache? importCache,
    Env? rootEnv,
    Context? context,
  }) => Runtime(
    env: env ?? this.env,
    stack: stack ?? this.stack,
    registry: registry ?? this.registry,
    importCache: importCache ?? this.importCache,
    rootEnv: rootEnv ?? this.rootEnv,
    context: context ?? this.context,
  );

  @override
  String toString() =>
      'Runtime(env: ${env.length} frames, stack: $stack, registry: ${registrySize(registry)} modules, cache: ${cacheSize(importCache)} imported, context: $context)';

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      (other is Runtime &&
          other.env == env &&
          _listsEqual(other.stack, stack) &&
          other.registry == registry &&
          other.importCache == importCache &&
          other.rootEnv == rootEnv &&
          other.context == context);

  @override
  int get hashCode =>
      Object.hash(env, stack, registry, importCache, rootEnv, context);
}

/// Helper function for list equality
bool _listsEqual(List<String> a, List<String> b) {
  if (identical(a, b)) return true;
  if (a.length != b.length) return false;
  for (int i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}
