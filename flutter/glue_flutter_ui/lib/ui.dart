import 'package:glue/module.dart';
import 'package:glue_flutter_ui/src/global_key.dart';
import 'package:glue_flutter_ui/src/styles/alignment.dart';
import 'package:glue_flutter_ui/src/styles/axis.dart';
import 'package:glue_flutter_ui/src/styles/blur_style.dart';
import 'package:glue_flutter_ui/src/styles/border_radius.dart';
import 'package:glue_flutter_ui/src/styles/border_side.dart';
import 'package:glue_flutter_ui/src/styles/border_style.dart';
import 'package:glue_flutter_ui/src/styles/box_border.dart';
import 'package:glue_flutter_ui/src/styles/box_constraints.dart';
import 'package:glue_flutter_ui/src/styles/box_decoration.dart';
import 'package:glue_flutter_ui/src/styles/box_fit.dart';
import 'package:glue_flutter_ui/src/styles/box_shadow.dart';
import 'package:glue_flutter_ui/src/styles/box_shape.dart';
import 'package:glue_flutter_ui/src/styles/brightness.dart';
import 'package:glue_flutter_ui/src/styles/clip.dart';
import 'package:glue_flutter_ui/src/styles/cross_axis_alignment.dart';
import 'package:glue_flutter_ui/src/styles/duration.dart';
import 'package:glue_flutter_ui/src/styles/edge_insets.dart';
import 'package:glue_flutter_ui/src/styles/filter_quality.dart';
import 'package:glue_flutter_ui/src/styles/flutter_logo_style.dart';
import 'package:glue_flutter_ui/src/styles/font_weight.dart';
import 'package:glue_flutter_ui/src/styles/gradient.dart';
import 'package:glue_flutter_ui/src/styles/hsl.dart';
import 'package:glue_flutter_ui/src/styles/hsla.dart';
import 'package:glue_flutter_ui/src/styles/hsv.dart';
import 'package:glue_flutter_ui/src/styles/hsva.dart';
import 'package:glue_flutter_ui/src/styles/image_filter.dart';
import 'package:glue_flutter_ui/src/styles/image_repeat.dart';
import 'package:glue_flutter_ui/src/styles/main_axis_alignment.dart';
import 'package:glue_flutter_ui/src/styles/main_axis_size.dart';
import 'package:glue_flutter_ui/src/styles/offset.dart';
import 'package:glue_flutter_ui/src/styles/radius.dart';
import 'package:glue_flutter_ui/src/styles/rgb.dart';
import 'package:glue_flutter_ui/src/styles/rgba.dart';
import 'package:glue_flutter_ui/src/styles/shape_border.dart';
import 'package:glue_flutter_ui/src/styles/stack_fit.dart';
import 'package:glue_flutter_ui/src/styles/system_brightness.dart';
import 'package:glue_flutter_ui/src/styles/text_align.dart';
import 'package:glue_flutter_ui/src/styles/text_baseline.dart';
import 'package:glue_flutter_ui/src/styles/text_direction.dart';
import 'package:glue_flutter_ui/src/styles/text_overflow.dart';
import 'package:glue_flutter_ui/src/styles/text_style.dart';
import 'package:glue_flutter_ui/src/styles/text_width_basis.dart';
import 'package:glue_flutter_ui/src/styles/tile_mode.dart';
import 'package:glue_flutter_ui/src/styles/vertical_direction.dart';
import 'package:glue_flutter_ui/src/styles/wrap_alignment.dart';
import 'package:glue_flutter_ui/src/styles/wrap_cross_alignment.dart';
import 'package:glue_flutter_ui/src/widgets/align.dart';
import 'package:glue_flutter_ui/src/widgets/animated_positioned.dart';
import 'package:glue_flutter_ui/src/widgets/animated_size.dart';
import 'package:glue_flutter_ui/src/widgets/aspect_ratio.dart';
import 'package:glue_flutter_ui/src/widgets/backdrop_filter.dart';
import 'package:glue_flutter_ui/src/widgets/center.dart';
import 'package:glue_flutter_ui/src/widgets/clip_rrect.dart';
import 'package:glue_flutter_ui/src/widgets/column.dart';
import 'package:glue_flutter_ui/src/widgets/container.dart';
import 'package:glue_flutter_ui/src/widgets/custom_scroll_view.dart';
import 'package:glue_flutter_ui/src/widgets/expanded.dart';
import 'package:glue_flutter_ui/src/widgets/fitted_box.dart';
import 'package:glue_flutter_ui/src/widgets/grid_view.dart';
import 'package:glue_flutter_ui/src/widgets/hero.dart';
import 'package:glue_flutter_ui/src/widgets/icon.dart';
import 'package:glue_flutter_ui/src/widgets/image.dart';
import 'package:glue_flutter_ui/src/widgets/image_filtered.dart';
import 'package:glue_flutter_ui/src/widgets/indexed_stack.dart';
import 'package:glue_flutter_ui/src/widgets/list_view.dart';
import 'package:glue_flutter_ui/src/widgets/opacity.dart';
import 'package:glue_flutter_ui/src/widgets/padding.dart';
import 'package:glue_flutter_ui/src/widgets/page_view.dart';
import 'package:glue_flutter_ui/src/widgets/placeholder.dart';
import 'package:glue_flutter_ui/src/widgets/pope_scope.dart';
import 'package:glue_flutter_ui/src/widgets/positioned.dart';
import 'package:glue_flutter_ui/src/widgets/repaint_boundary.dart';
import 'package:glue_flutter_ui/src/widgets/row.dart';
import 'package:glue_flutter_ui/src/widgets/safe_area.dart';
import 'package:glue_flutter_ui/src/widgets/single_child_scroll_view.dart';
import 'package:glue_flutter_ui/src/widgets/sized_box.dart';
import 'package:glue_flutter_ui/src/widgets/sliver_grid.dart';
import 'package:glue_flutter_ui/src/widgets/sliver_list.dart';
import 'package:glue_flutter_ui/src/widgets/spacer.dart';
import 'package:glue_flutter_ui/src/widgets/stack.dart';
import 'package:glue_flutter_ui/src/widgets/text.dart';
import 'package:glue_flutter_ui/src/widgets/wrap.dart';

final ModuleInfo uiCoreModule = nativeModule('ffi.ui.core', [
  // Core widget functions
  ('align', align),
  ('animated-positioned', animatedPositioned),
  ('animated-size', animatedSize),
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
  ('image', image),
  ('image-asset', imageAsset),
  ('image-file', imageFile),
  ('image-network', imageNetwork),
  ('indexed-stack', indexedStack),
  ('hero', hero),
  ('list-view', listView),
  ('opacity', opacity),
  ('padding', padding),
  ('page-view', pageView),
  ('placeholder', placeholder),
  ('positioned', positioned),
  ('pop-scope', popScope),
  ('repaint-boundary', repaintBoundary),
  ('row', row),
  ('safe-area', safeArea),
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

  ('box-decoration', boxDecoration),
  ('box-shadow', boxShadow),

  // Edge insets creation functions
  ('edge-insets-all', edgeInsetsAll),
  ('edge-insets-symmetric', edgeInsetsSymmetric),
  ('edge-insets-only', edgeInsetsOnly),
  ('edge-insets-directional', edgeInsetsDirectional),

  // Border radius functions
  ('border-radius-all', borderRadiusAll),
  ('border-radius-circular', borderRadiusCircular),
  ('border-radius-only', borderRadiusOnly),
  ('border-radius-horizontal', borderRadiusHorizontal),
  ('border-radius-vertical', borderRadiusVertical),
  ('border-radius-directional', borderRadiusDirectional),
  ('border-radius-directional-horizontal', borderRadiusDirectionalHorizontal),
  ('border-radius-zero', borderRadiusZero),

  //Border functions
  ('border-all', borderAll),
  ('border-only', borderOnly),
  ('border-symmetric', borderSymmetric),

  // Shape border functions
  ('border-rounded-rectangle', borderRoundedRectangle),
  ('border-circle', borderCircle),
  ('border-stadium', borderStadium),
  ('border-beveled-rectangle', borderBeveledRectangle),

  ('border-side', borderSide),
  ('border-style', borderStyle),

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
  ('blur-style', blurStyle),
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
  ('box-shape', boxShape),
  ('image-repeat', imageRepeat),
  ('text-overflow', textOverflow),
  ('text-width-basis', textWidthBasis),
  ('filter-quality', filterQuality),
  ('axis', axis),

  ('brightness', brightness),
  ('system-brightness', systemBrightness),

  ('linear-gradient', linearGradient),
  ('radial-gradient', radialGradient),
  ('sweep-gradient', sweepGradient),

  ('tile-mode', tileMode),

  ('stack-fit', stackFit),

  ('global-key', globalKey),

  ('backdrop-filter', backdropFilter),
  ('image-filtered', imageFiltered),
  ('image-filter-blur', imageFilterBlur),
]);
