import 'runtime_exceptions.dart';

/// Evaluation error handling system
/// Mirrors Haskell Glue.Eval.Error exactly

/// Call stack context for error reporting
typedef Context = List<String>;

/// Evaluation error wrapping runtime exception with context
class EvalError {
  final Context context;
  final RuntimeException exception;

  const EvalError(this.context, this.exception);

  @override
  String toString() => '$context: $exception';

  @override
  bool operator ==(Object other) =>
      other is EvalError &&
      _listsEqual(other.context, context) &&
      other.exception == exception;

  @override
  int get hashCode => Object.hash(context, exception);
}

/// Pretty-print evaluation error with context
String prettyShow(EvalError error) {
  if (error.context.isEmpty) {
    return error.exception.pretty();
  }
  final contextStr = error.context.reversed.join(' -> ');
  return '$contextStr: ${error.exception.pretty()}';
}

/// Helper function for list equality
bool _listsEqual(List<String> a, List<String> b) {
  if (a.length != b.length) return false;
  for (int i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}
