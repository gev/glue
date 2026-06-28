import 'package:glue/module.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/axis.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/box_fit.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/brightness.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/clip.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/cross_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/edge_insets.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/filter_quality.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/flutter_logo_style.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/font_weight.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/image_repeat.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/main_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/main_axis_size.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/rgb.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/rgba.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/shape_border.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/system_brightness.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_align.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_baseline.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_direction.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_overflow.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_style.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_width_basis.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/vertical_direction.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/aspect_ratio.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/center.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/column.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/container.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/custom_scroll_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/expanded.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/grid_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/icon.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/image.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/list_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/padding.dart';
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
  ('aspect-ratio', aspectRatio),
  ('center', center),
  ('column', column),
  ('container', container),
  ('custom-scroll-view', customScrollView),
  ('expanded', expanded),
  ('grid-view', gridView),
  ('icon', icon),
  ('image-asset', imageAsset),
  ('image-file', imageFile),
  ('image-network', imageNetwork),
  ('list-view', listView),
  ('padding', padding),
  ('placeholder', placeholder),
  ('row', row),
  ('single-child-scroll-view', singleChildScrollView),
  ('sized-box', sizedBox),
  ('sliver-grid', sliverGrid),
  ('sliver-list', sliverList),
  ('text', text),
  ('wrap', wrap),

  // Color creation functions
  ('rgb', rgb),
  ('rgba', rgba),

  // Shape border functions
  ('border-rounded-rectangle', borderRoundedRectangle),
  ('border-circle', borderCircle),
  ('border-stadium', borderStadium),
  ('border-beveled-rectangle', borderBeveledRectangle),
  ('border-radius-circular', borderRadiusCircular),
  ('border-radius-only', borderRadiusOnly),
  ('border-radius-val', borderRadiusVal),

  // Text style creation function
  ('text-style', textStyle),

  // Padding creation functions
  ('edge-insets-all', edgeInsetsAll),
  ('edge-insets-symmetric', edgeInsetsSymmetric),
  ('edge-insets-only', edgeInsetsOnly),
  ('edge-insets-directional', edgeInsetsDirectional),

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
