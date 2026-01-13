import 'package:fast_immutable_collections/fast_immutable_collections.dart';

import 'ir.dart';
import 'runtime_exceptions.dart';

/// Environment types for Glue evaluation
/// Mirrors Haskell Glue.Env exactly

/// Environment is a stack of frames, searched top to bottom
typedef Env = IList<Frame>;

/// Frame is a mapping from symbol names to IR values
typedef Frame = IMap<String, Ir>;

/// Create empty environment stack
Env emptyEnv() => IList<Frame>();

/// Create environment from list of bindings
Env fromList(List<(String, Ir)> pairs) => IList<Frame>([
  IMap<String, Ir>.fromEntries(pairs.map((e) => MapEntry(e.$1, e.$2))),
]);

/// Create frame from list of bindings
Frame frameFromList(List<(String, Ir)> pairs) =>
    IMap<String, Ir>.fromEntries(pairs.map((e) => MapEntry(e.$1, e.$2)));

/// Create environment from single frame
Env fromFrame(Frame frame) => IList<Frame>([frame]);

/// Push empty frame onto environment stack
Env pushFrame(Env env) => env.add(IMap<String, Ir>());

/// Pop frame from environment stack (safe - returns empty env if already empty)
Env popFrame(Env env) => env.isNotEmpty ? env.removeLast() : IList<Frame>();

/// Lookup variable in local (top) frame only
Ir? lookupLocal(String name, Env env) => env.isNotEmpty ? env.last[name] : null;

/// Lookup variable in entire environment stack
/// Returns `Either<RuntimeException, Ir>` to match Haskell
(RuntimeException?, Ir?) lookupVar(String name, Env env) {
  // Search from top frame (most recent) to bottom (oldest)
  for (var i = env.length - 1; i >= 0; i--) {
    final frame = env[i];
    final value = frame[name];
    if (value != null) {
      return (null, value);
    }
  }
  return (unboundVariable(name), null);
}

/// Define variable in current (top) frame
/// Creates new frame if environment is empty
Env defineVar(String name, Ir value, Env env) {
  if (env.isEmpty) {
    return IList<Frame>([
      IMap<String, Ir>({name: value}),
    ]);
  } else {
    final newFrame = env.last.add(name, value);
    return env.removeLast().add(newFrame);
  }
}

/// Update existing variable in environment
/// Searches through frames and updates first occurrence (top to bottom)
/// Returns `Either<RuntimeException, Env>` to match Haskell
(RuntimeException?, Env?) updateVar(String name, Ir value, Env env) {
  // Search from top frame (most recent) to bottom (oldest)
  for (var i = env.length - 1; i >= 0; i--) {
    final frame = env[i];
    if (frame.containsKey(name)) {
      final newFrame = frame.add(name, value);
      final newEnv = env.removeAt(i).insert(i, newFrame);
      return (null, newEnv);
    }
  }
  return (canNotSetUnboundVariable(name), null);
}

/// Union multiple frames into single frame
/// Later frames override earlier ones
Frame unionFrames(List<Frame> frames) {
  var result = IMap<String, Ir>();
  for (final frame in frames) {
    result = result.addAll(IMap<String, Ir>(frame.unlock));
  }
  return result;
}
