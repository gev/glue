import 'package:glue/src/error.dart';
import 'package:glue/src/eval/exception.dart';

/// Evaluation error handling system
/// Mirrors Haskell Glue.Eval.Error exactly

/// Call stack stack for error reporting
typedef CallStack = List<String>;

/// Evaluation error wrapping runtime exception with stack
class EvalError implements GlueError {
  final CallStack stack;
  final RuntimeException exception;

  const EvalError(this.stack, this.exception);

  @override
  String pretty() => prettyShow(this);

  @override
  String toString() => '$stack: $exception';

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      (other is EvalError &&
          _listsEqual(other.stack, stack) &&
          other.exception == exception);

  @override
  int get hashCode => Object.hash(stack, exception);
}

/// Pretty-print evaluation error with stack
String prettyShow(EvalError error) {
  if (error.stack.isEmpty) {
    return error.exception.pretty();
  }
  final contextStr = error.stack.reversed.join(' -> ');
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
