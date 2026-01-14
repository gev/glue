/// Top-level error type for Glue operations
/// Mirrors Haskell GlueError exactly - single interface for all errors
abstract interface class GlueError {
  String pretty();
}
