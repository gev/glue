import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/ref.dart';

/// Environment types for Glue evaluation
/// Mirrors Haskell Glue.Env exactly

/// Environment is a stack of frames, searched top to bottom
typedef Env = IList<Frame>;

/// Frame is a mapping from symbol names to IR values
typedef Frame = IMap<String, Ref<Ir>>;

/// Create empty environment stack
Env emptyEnv() => Env();

/// Create environment from list of bindings
Env fromList(List<(String, Ir)> pairs) => Env([
  IMap.fromEntries(
    pairs.map((pair) {
      final (key, value) = pair;
      return MapEntry(key, Ref(value));
    }),
  ),
]);

/// Create frame from list of bindings
Frame frameFromList(List<(String, Ref<Ir>)> pairs) => IMap.fromEntries(
  pairs.map((pair) {
    final (key, value) = pair;
    return MapEntry(key, value);
  }),
);

/// Create environment from single frame
Env fromFrame(Frame frame) => Env([frame]);

/// Push empty frame onto environment stack
Env pushFrame(Env env) => env.add(IMap());

/// Pop frame from environment stack (safe - returns empty env if already empty)
Env popFrame(Env env) => env.isNotEmpty ? env.removeLast() : Env();

/// Lookup variable in local (top) frame only
Ir? lookupLocal(String name, Env env) =>
    env.isNotEmpty ? env.last[name]?.value : null;

/// Lookup variable in entire environment stack
/// Returns `Either<RuntimeException, Ir>` to match Haskell
Either<RuntimeException, Ir> lookupVar(String name, Env env) {
  // Search from top frame (most recent) to bottom (oldest)
  for (var i = env.length - 1; i >= 0; i--) {
    final frame = env[i];
    final value = frame[name];
    if (value != null) {
      return Right(value.value);
    }
  }
  return Left(unboundVariable(name));
}

/// Define reference in current (top) frame
/// Creates new frame if environment is empty
Env defineRef(String name, Ref<Ir> value, Env env) {
  print(value);
  if (env.isEmpty) {
    return Env([
      IMap({name: value}),
    ]);
  } else {
    final newFrame = env.last.add(name, value);
    return env.removeLast().add(newFrame);
  }
}

Env defineVar(String name, Ir value, Env env) =>
    defineRef(name, Ref(value), env);

/// Union multiple frames into single frame
/// Later frames override earlier ones
Frame unionFrames(List<Frame> frames) {
  var result = Frame();
  for (final frame in frames) {
    result = result.addAll(Frame(frame.unlock));
  }
  return result;
}
