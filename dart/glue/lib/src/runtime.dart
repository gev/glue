import 'env.dart';
import 'eval_error.dart';
import 'module_cache.dart';
import 'module_registry.dart';

/// Runtime state for Glue evaluation
/// Mirrors Haskell Glue.Eval.Runtime exactly

/// Complete evaluation runtime containing all state
class Runtime {
  final Env env;
  final Context context;
  final ModuleRegistry registry;
  final ImportedModuleCache importCache;
  final Env rootEnv;

  const Runtime({
    required this.env,
    required this.context,
    required this.registry,
    required this.importCache,
    required this.rootEnv,
  });

  /// Create initial runtime with empty module system
  factory Runtime.initial(Env initialEnv) => Runtime(
    env: initialEnv,
    context: [],
    registry: emptyRegistry(),
    importCache: emptyCache(),
    rootEnv: initialEnv,
  );

  /// Create a copy with modified fields
  Runtime copyWith({
    Env? env,
    Context? context,
    ModuleRegistry? registry,
    ImportedModuleCache? importCache,
    Env? rootEnv,
  }) => Runtime(
    env: env ?? this.env,
    context: context ?? this.context,
    registry: registry ?? this.registry,
    importCache: importCache ?? this.importCache,
    rootEnv: rootEnv ?? this.rootEnv,
  );

  @override
  String toString() =>
      'Runtime(env: ${env.length} frames, context: $context, registry: ${registrySize(registry)} modules, cache: ${cacheSize(importCache)} imported)';

  @override
  bool operator ==(Object other) =>
      other is Runtime &&
      other.env == env &&
      _listsEqual(other.context, context) &&
      other.registry == registry &&
      other.importCache == importCache &&
      other.rootEnv == rootEnv;

  @override
  int get hashCode => Object.hash(env, context, registry, importCache, rootEnv);
}

/// Helper function for list equality
bool _listsEqual(List<String> a, List<String> b) {
  if (a.length != b.length) return false;
  for (int i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}
