import 'package:glue/module.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/alignment.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/axis.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/border_rdius.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/box_constraints.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/box_fit.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/brightness.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/clip.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/cross_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/duration.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/edge_insets.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/filter_quality.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/flutter_logo_style.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/font_weight.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/hsl.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/hsla.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/hsv.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/hsva.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/image_repeat.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/main_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/main_axis_size.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/offset.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/radius.dart';
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
import 'package:glue_flutter/src/lib/ui/core/styles/wrap_alignment.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/wrap_cros_alignment.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/align.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/animated_positioned.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/aspect_ratio.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/center.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/clip_rrect.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/column.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/container.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/custom_scroll_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/expanded.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/fitted_box.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/grid_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/hero.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/icon.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/image.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/list_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/padding.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/placeholder.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/positioned.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/row.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/single_child_scroll_view.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/sized_box.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/sliver_grid.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/sliver_list.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/spacer.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/stack.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/text.dart';
import 'package:glue_flutter/src/lib/ui/core/widgets/wrap.dart';

final ModuleInfo uiCoreModule = nativeModule('ffi.ui.core', [
  // Core widget functions
  ('align', align),
  ('animated-positioned', animatedPositioned),
  ('aspect-ratio', aspectRatio),
  ('center', center),
  ('clip-rrect', clipRRect),
  ('column', column),
  ('container', container),
  ('custom-scroll-view', customScrollView),
  ('expanded', expanded),
  ('fitted-box', fittedBox),
  ('grid-view', gridView),
  ('icon', icon),
  ('image-asset', imageAsset),
  ('image-file', imageFile),
  ('image-network', imageNetwork),
  ('hero', hero),
  ('list-view', listView),
  ('padding', padding),
  ('positioned', positioned),
  ('placeholder', placeholder),
  ('row', row),
  ('single-child-scroll-view', singleChildScrollView),
  ('sized-box', sizedBox),
  ('spacer', spacer),
  ('sliver-grid', sliverGrid),
  ('sliver-list', sliverList),
  ('stack', stack),
  ('text', text),
  ('wrap', wrap),

  // Color creation functions
  ('rgb', rgb),
  ('rgba', rgba),
  ('hsl', hsl),
  ('hsla', hsla),
  ('hsv', hsv),
  ('hsva', hsva),

  //Duration function
  ('duration', duration),
  // Text style creation function
  ('text-style', textStyle),

  // Box constraints functions
  ('box-constraints', boxConstraintsOnly),
  ('box-constraints-tight', boxConstraintsTight),

  // Edge insets creation functions
  ('edge-insets-all', edgeInsetsAll),
  ('edge-insets-symmetric', edgeInsetsSymmetric),
  ('edge-insets-only', edgeInsetsOnly),
  ('edge-insets-directional', edgeInsetsDirectional),

  // Broder radius functions
  ('border-radius-all', borderRadiusAll),
  ('border-radius-circular', borderRadiusCircular),
  ('border-radius-only', borderRadiusOnly),
  ('border-radius-horizontal', borderRadiusHorizontal),
  ('border-radius-vertical', borderRadiusVertical),
  ('border-radius-directional', borderRadiusDirectional),
  ('border-radius-directional-horizontal', borderRadiusDirectionalHorizontal),
  ('border-radius-zero', borderRadiusZero),

  // Shape border functions
  ('border-rounded-rectangle', borderRoundedRectangle),
  ('border-circle', borderCircle),
  ('border-stadium', borderStadium),
  ('border-beveled-rectangle', borderBeveledRectangle),

  // Radius functions
  ('radius', radiusCircular),
  ('radius-circular', radiusCircular),
  ('radius-elliptical', radiusElliptical),
  ('radius-zero', radiusZero),

  //Offset function
  ('offset-zero', offsetZero),
  ('offset-infinite', offsetInfinite),
  ('offset', offset),

  // Enum union objects
  ('alignment', alignment),
  ('cross-axis-alignment', crossAxisAlignment),
  ('main-axis-alignment', mainAxisAlignment),
  ('main-axis-size', mainAxisSize),
  ('wrap-alignment', wrapAlignment),
  ('wrap-cross-alignment', wrapCrossAlignment),
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
