/// Top-level error type for Glue operations
/// Mirrors Haskell GlueError exactly - single interface for all errors
abstract interface class GlueError {
  String pretty();
}

/// Convenience function to create GlueError
/// Mirrors Haskell's GlueError constructor
GlueError glueError(dynamic error) {
  if (error is GlueError) {
    return error;
  }
  // For non-GlueError types, we could wrap them, but for now assume they're already GlueError
  throw ArgumentError('Expected GlueError, got ${error.runtimeType}');
}
