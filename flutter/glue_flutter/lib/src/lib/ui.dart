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
import 'package:glue_flutter/src/lib/ui/font_weight.dart';
import 'package:glue_flutter/src/lib/ui/color/colors.dart'; // Named colors object
import 'package:glue_flutter/src/lib/ui/color/rgb.dart'; // RGB function
import 'package:glue_flutter/src/lib/ui/color/rgba.dart'; // RGBA function
import 'package:glue_flutter/src/lib/ui/padding/padding_all.dart'; // padding-all function
import 'package:glue_flutter/src/lib/ui/padding/padding_symmetric.dart'; // padding-symmetric function
import 'package:glue_flutter/src/lib/ui/padding/padding_only.dart'; // padding-only function
import 'package:glue_flutter/src/lib/ui/padding/padding_directional.dart'; // padding-directional function

// Note: All exports are handled through the Glue ModuleInfo below.
// No Dart re-exports to keep the module boundary clean.

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

  // Color creation functions
  ('rgb', rgb),
  ('rgba', rgba),

  // Padding creation functions
  ('padding-all', paddingAll),
  ('padding-symmetric', paddingSymmetric),
  ('padding-only', paddingOnly),
  ('padding-directional', paddingDirectional),

  // Enum union objects
  ('cross-axis-alignment', crossAxisAlignment),
  ('main-axis-alignment', mainAxisAlignment),
  ('text-align', textAlign),
  ('font-weight', fontWeight),
  ('colors', colors),
]);
