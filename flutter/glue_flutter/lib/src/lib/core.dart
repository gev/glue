import 'package:glue/module.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/axis.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/box_fit.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/brightness.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/clip.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/cross_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/filter_quality.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/flutter_logo_style.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/font_weight.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/image_repeat.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/main_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/main_axis_size.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/padding_all.dart'; // padding-all function
import 'package:glue_flutter/src/lib/ui/core/styles/padding_directional.dart'; // padding-directional function
import 'package:glue_flutter/src/lib/ui/core/styles/padding_only.dart'; // padding-only function
import 'package:glue_flutter/src/lib/ui/core/styles/padding_symmetric.dart'; // padding-symmetric function
import 'package:glue_flutter/src/lib/ui/core/styles/rgb.dart'; // RGB function
import 'package:glue_flutter/src/lib/ui/core/styles/rgba.dart'; // RGBA function
import 'package:glue_flutter/src/lib/ui/core/styles/system_brightness.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_align.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_baseline.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_direction.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_overflow.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_style.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_width_basis.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/vertical_direction.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/center.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/column.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/container.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/custom_scroll_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/expanded.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/grid_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/icon.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/image.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/list_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/placeholder.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/row.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/single_child_scroll_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/sized_box.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/sliver_grid.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/sliver_list.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/text.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/wrap.dart';

final ModuleInfo uiCoreModule = nativeModule('ffi.ui.core', [
  // Core widget functions
  ('container', container),
  ('column', column),
  ('row', row),
  ('wrap', wrap),
  ('center', center),
  ('sized-box', sizedBox),
  ('expanded', expanded),
  ('icon', icon),
  ('placeholder', placeholder),
  ('image', image),
  ('list-view', listView),
  ('grid-view', gridView),
  ('single-child-scroll-view', singleChildScrollView),
  ('custom-scroll-view', customScrollView),
  ('sliver-list', sliverList),
  ('sliver-grid', sliverGrid),
  ('text', text),

  // Color creation functions
  ('rgb', rgb),
  ('rgba', rgba),

  // Text style creation function
  ('text-style', textStyle),

  // Padding creation functions
  ('padding-all', paddingAll),
  ('padding-symmetric', paddingSymmetric),
  ('padding-only', paddingOnly),
  ('padding-directional', paddingDirectional),

  // Enum union objects
  ('cross-axis-alignment', crossAxisAlignment),
  ('main-axis-alignment', mainAxisAlignment),
  ('main-axis-size', mainAxisSize),
  ('text-align', textAlign),
  ('text-direction', textDirection),
  ('vertical-direction', verticalDirection),
  ('text-baseline', textBaseline),
  ('clip', clip),
  ('font-weight', fontWeight),
  ('flutter-logo-style', flutterLogoStyle),
  ('box-fit', boxFit),
  ('image-repeat', imageRepeat),
  ('text-overflow', textOverflow),
  ('text-width-basis', textWidthBasis),
  ('filter-quality', filterQuality),
  ('axis', axis),

  ('brightness', brightness),
  ('system-brightness', systemBrightness),
]);
