import 'package:glue/src/module.dart';
import 'lib/ui/text.dart';
import 'lib/ui/button.dart';
import 'lib/ui/container.dart';
import 'lib/ui/column.dart';
import 'lib/ui/row.dart';
import 'lib/ui/padding.dart';
import 'lib/ui/center.dart';

// Export individual functions for testing and external access
export 'lib/ui/text.dart' show text;
export 'lib/ui/button.dart' show button;
export 'lib/ui/container.dart' show container;
export 'lib/ui/column.dart' show column;
export 'lib/ui/row.dart' show row;
export 'lib/ui/padding.dart' show padding;
export 'lib/ui/center.dart' show center;

/// UI module - Flutter implementation of framework-agnostic UI API
/// Provides concrete Flutter rendering for abstract UI specifications

/// The ui module containing all UI functions
/// Implements the framework-agnostic UI API with Flutter widgets
final ModuleInfo ui = nativeModule('ui', [
  // Core widget functions
  ('text', text),
  ('button', button),
  ('container', container),
  ('column', column),
  ('row', row),
  ('padding', padding),
  ('center', center),
]);
