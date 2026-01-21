import 'package:glue/src/module.dart';
import 'package:glue_flutter/src/lib/ui/text.dart';
import 'package:glue_flutter/src/lib/ui/button.dart';
import 'package:glue_flutter/src/lib/ui/container.dart';
import 'package:glue_flutter/src/lib/ui/column.dart';
import 'package:glue_flutter/src/lib/ui/row.dart';
import 'package:glue_flutter/src/lib/ui/padding.dart';
import 'package:glue_flutter/src/lib/ui/center.dart';
import 'package:glue_flutter/src/lib/ui/cross_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/main_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/text_align.dart';

// Export individual functions for testing and external access
export 'package:glue_flutter/src/lib/ui/text.dart' show text;
export 'package:glue_flutter/src/lib/ui/button.dart' show button;
export 'package:glue_flutter/src/lib/ui/container.dart' show container;
export 'package:glue_flutter/src/lib/ui/column.dart' show column;
export 'package:glue_flutter/src/lib/ui/row.dart' show row;
export 'package:glue_flutter/src/lib/ui/padding.dart' show padding;
export 'package:glue_flutter/src/lib/ui/center.dart' show center;

// Export enum objects
export 'package:glue_flutter/src/lib/ui/cross_axis_alignment.dart'
    show crossAxisAlignment;
export 'package:glue_flutter/src/lib/ui/main_axis_alignment.dart'
    show mainAxisAlignment;
export 'package:glue_flutter/src/lib/ui/text_align.dart' show textAlign;

/// UI module - Flutter implementation of framework-agnostic UI API
/// Provides concrete Flutter rendering for abstract UI specifications

/// The ui module containing all UI functions and enum objects
/// Implements the framework-agnostic UI API with Flutter widgets and enum unions
final ModuleInfo ui = nativeModule('ui', [
  // Core widget functions
  ('text', text),
  ('button', button),
  ('container', container),
  ('column', column),
  ('row', row),
  ('padding', padding),
  ('center', center),

  // Enum union objects
  ('cross-axis-alignment', crossAxisAlignment),
  ('main-axis-alignment', mainAxisAlignment),
  ('text-align', textAlign),
]);
