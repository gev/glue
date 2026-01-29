import 'package:flutter/gestures.dart';
import 'package:flutter/services.dart';
import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';
import 'package:glue/runtime.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Properties wrapper with lazy getters
/// Provides clean API for accessing widget properties without pre-computing everything
class CoreProperties {
  final Map<String, Ir> _props;

  Ir? prop(String key) => _props[key];

  CoreProperties(Map<String, Ir> props) : _props = props;
  CoreProperties.empty() : _props = {};

  // Padding properties
  double? get top => extractDouble(prop('top'));
  double? get bottom => extractDouble(prop('bottom'));
  double? get start => extractDouble(prop('start'));
  double? get end => extractDouble(prop('end'));
  double? get left => extractDouble(prop('left'));
  double? get right => extractDouble(prop('right'));
  double? get vertical => extractDouble(prop('vertical'));
  double? get horizontal => extractDouble(prop('horizontal'));

  // Button properties
  VoidCallback? onPress(Runtime runtime) =>
      extractVoidCallback(prop('on-press'), runtime);
  VoidCallback? onLongPress(Runtime runtime) =>
      extractVoidCallback(prop('on-long-press'), runtime);

  // Text properties
  String? get content => extractString(prop('content'));
  Color? get color => extractColor(prop('color'));
  double? get size => extractDouble(prop('size'));
  FontWeight? get weight => extractNativeValue<FontWeight>(prop('weight'));
  TextAlign? get align => extractNativeValue<TextAlign>(prop('align'));
  TextOverflow? get overflow =>
      extractNativeValue<TextOverflow>(prop('overflow'));
  double? get textScaleFactor => extractDouble(prop('text-scale-factor'));
  int? get maxLines => extractInt(prop('max-lines'));
  String? get semanticsLabel => extractString(prop('semantics-label'));
  TextWidthBasis? get textWidthBasis =>
      extractNativeValue<TextWidthBasis>(prop('text-width-basis'));
  TextHeightBehavior? get textHeightBehavior =>
      extractNativeValue<TextHeightBehavior>(prop('text-height-behavior'));
  bool? get softWrap => extractBool(prop('soft-wrap'));
  Locale? get locale => extractNativeValue<Locale>(prop('locale'));
  StrutStyle? get strutStyle =>
      extractNativeValue<StrutStyle>(prop('strut-style'));
  TextStyle? get textStyle => extractNativeValue<TextStyle>(prop('text-style'));

  // Layout properties
  List<Widget> get children => extractChildren(prop('children')) ?? [];
  Widget? get child => extractNativeValue<Widget>(prop('child'));
  MainAxisAlignment get mainAlign =>
      extractNativeValue<MainAxisAlignment>(prop('main-axis-align')) ??
      MainAxisAlignment.start;
  CrossAxisAlignment get crossAlign =>
      extractNativeValue<CrossAxisAlignment>(prop('cross-axis-align')) ??
      CrossAxisAlignment.start;
  Axis get direction =>
      extractNativeValue<Axis>(prop('direction')) ?? Axis.vertical;
  double? get spacing => extractDouble(prop('spacing'));
  MainAxisSize get mainAxisSize =>
      extractNativeValue<MainAxisSize>(prop('main-axis-size')) ??
      MainAxisSize.max;
  TextDirection? get textDirection =>
      extractNativeValue<TextDirection>(prop('text-direction'));
  VerticalDirection get verticalDirection =>
      extractNativeValue<VerticalDirection>(prop('vertical-direction')) ??
      VerticalDirection.down;
  TextBaseline? get textBaseline =>
      extractNativeValue<TextBaseline>(prop('text-baseline'));

  // Icon properties
  IconData? get icon => extractNativeValue<IconData>(prop('icon'));

  // Image properties
  ImageProvider? get imageProvider =>
      extractNativeValue<ImageProvider>(prop('image'));
  BoxFit? get boxFit => extractNativeValue<BoxFit>(prop('fit'));
  BlendMode? get blendMode =>
      extractNativeValue<BlendMode>(prop('color-blend-mode'));
  ImageRepeat? get imageRepeat =>
      extractNativeValue<ImageRepeat>(prop('repeat'));
  bool? get matchTextDirection => extractBool(prop('match-text-direction'));
  bool? get gaplessPlayback => extractBool(prop('gapless-playback'));
  bool? get excludeFromSemantics => extractBool(prop('exclude-from-semantics'));
  FilterQuality? get filterQuality =>
      extractNativeValue<FilterQuality>(prop('filter-quality'));
  int? get cacheWidth => extractInt(prop('cache-width'));
  int? get cacheHeight => extractInt(prop('cache-height'));

  // TextField properties
  TextEditingController? get textEditingController =>
      extractNativeValue<TextEditingController>(prop('controller'));
  TextInputType? get keyboardType =>
      extractNativeValue<TextInputType>(prop('keyboard-type'));
  TextInputAction? get textInputAction =>
      extractNativeValue<TextInputAction>(prop('text-input-action'));
  TextCapitalization get textCapitalization =>
      extractNativeValue<TextCapitalization>(prop('text-capitalization')) ??
      TextCapitalization.none;
  TextAlignVertical? get textAlignVertical =>
      extractNativeValue<TextAlignVertical>(prop('text-align-vertical'));
  bool get readOnly => extractBool(prop('read-only')) ?? false;
  bool get textFieldAutofocus => extractBool(prop('autofocus')) ?? false;
  String get obscuringCharacter =>
      extractString(prop('obscuring-character')) ?? '•';
  bool get obscureText => extractBool(prop('obscure-text')) ?? false;
  bool get enableSuggestions => extractBool(prop('enable-suggestions')) ?? true;
  int? get textFieldMaxLines => extractInt(prop('max-lines'));
  int? get minLines => extractInt(prop('min-lines'));
  bool get expands => extractBool(prop('expands')) ?? false;
  int? get maxLength => extractInt(prop('max-length'));
  MaxLengthEnforcement? get maxLengthEnforcement =>
      extractNativeValue<MaxLengthEnforcement>(prop('max-length-enforcement'));
  ValueChanged<String>? get onTextChanged =>
      extractNativeValue<ValueChanged<String>>(prop('on-changed'));
  VoidCallback? onEditingComplete(Runtime runtime) =>
      extractVoidCallback(prop('on-editing-complete'), runtime);
  ValueChanged<String>? get onSubmitted =>
      extractNativeValue<ValueChanged<String>>(prop('on-submitted'));
  List<TextInputFormatter>? get inputFormatters =>
      extractNativeValue<List<TextInputFormatter>>(prop('input-formatters'));
  bool? get textFieldEnabled => extractBool(prop('enabled'));
  double get cursorWidth => extractDouble(prop('cursor-width')) ?? 2.0;
  double? get cursorHeight => extractDouble(prop('cursor-height'));
  Radius? get cursorRadius => extractNativeValue<Radius>(prop('cursor-radius'));
  Color? get cursorColor => extractColor(prop('cursor-color'));
  Color? get cursorErrorColor => extractColor(prop('cursor-error-color'));
  Brightness? get keyboardAppearance =>
      extractNativeValue<Brightness>(prop('keyboard-appearance'));
  EdgeInsets get textFieldScrollPadding =>
      extractNativeValue<EdgeInsets>(prop('scroll-padding')) ??
      const EdgeInsets.all(20.0);
  bool? get enableInteractiveSelection =>
      extractBool(prop('enable-interactive-selection'));
  bool? get selectAllOnFocus => extractBool(prop('select-all-on-focus'));
  TextSelectionControls? get selectionControls =>
      extractNativeValue<TextSelectionControls>(prop('selection-controls'));
  GestureTapCallback? get onTextFieldTap =>
      extractNativeValue<GestureTapCallback>(prop('on-press'));
  bool get onTapAlwaysCalled =>
      extractBool(prop('on-tap-always-called')) ?? false;
  MouseCursor? get textFieldMouseCursor =>
      extractNativeValue<MouseCursor>(prop('mouse-cursor'));
  ScrollController? get textFieldScrollController =>
      extractNativeValue<ScrollController>(prop('scroll-controller'));
  ScrollPhysics? get scrollPhysics =>
      extractNativeValue<ScrollPhysics>(prop('scroll-physics'));
  Iterable<String>? get autofillHints =>
      extractNativeValue<Iterable<String>>(prop('autofill-hints'));
  String? get textFieldRestorationId => extractString(prop('restoration-id'));

  // Switch properties
  bool get switchValue => extractBool(prop('value')) ?? false;
  ValueChanged<bool>? get onSwitchChanged =>
      extractNativeValue<ValueChanged<bool>>(prop('on-changed'));
  Color? get activeThumbColor => extractColor(prop('active-thumb-color'));
  Color? get inactiveThumbColor => extractColor(prop('inactive-thumb-color'));
  Color? get activeTrackColor => extractColor(prop('active-track-color'));
  Color? get inactiveTrackColor => extractColor(prop('inactive-track-color'));
  ImageProvider<Object>? get activeThumbImage =>
      extractNativeValue<ImageProvider<Object>>(prop('active-thumb-image'));
  ImageErrorListener? get onActiveThumbImageError =>
      extractNativeValue<ImageErrorListener>(
        prop('on-active-thumb-image-error'),
      );
  ImageProvider<Object>? get inactiveThumbImage =>
      extractNativeValue<ImageProvider<Object>>(prop('inactive-thumb-image'));
  ImageErrorListener? get onInactiveThumbImageError =>
      extractNativeValue<ImageErrorListener>(
        prop('on-inactive-thumb-image-error'),
      );
  WidgetStateProperty<Color?>? get thumbColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('thumb-color'));
  WidgetStateProperty<Color?>? get trackColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('track-color'));
  WidgetStateProperty<Color?>? get trackOutlineColor =>
      extractNativeValue(prop('track-outline-color'));
  WidgetStateProperty<double?>? get trackOutlineWidth =>
      extractNativeValue(prop('track-outline-width'));
  WidgetStateProperty<Icon?>? get thumbIcon =>
      extractNativeValue(prop('thumb-icon'));
  EdgeInsetsGeometry? get switchPadding => extractNativeValue(prop('padding'));

  // AlertDialog properties
  List<Widget>? get alertDialogActions => extractChildren(prop('actions'));

  // Slider properties
  DragStartBehavior? get drawerDragStartBehavior =>
      extractNativeValue<DragStartBehavior>(prop('drawer-drag-start-behavior'));

  // Container properties
  EdgeInsetsGeometry get padding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding')) ??
      EdgeInsets.zero;
  AlignmentGeometry? get alignment =>
      extractNativeValue<AlignmentGeometry>(prop('alignment'));
  double? get width => extractDouble(prop('width'));
  double? get height => extractDouble(prop('height'));
  BoxConstraints? get constraints =>
      extractNativeValue<BoxConstraints>(prop('constraints'));
  EdgeInsetsGeometry? get margin =>
      extractNativeValue<EdgeInsetsGeometry>(prop('margin'));
  Decoration? get decoration =>
      extractNativeValue<Decoration>(prop('decoration'));
  Decoration? get foregroundDecoration =>
      extractNativeValue<Decoration>(prop('foreground-decoration'));
  Matrix4? get transform => extractNativeValue<Matrix4>(prop('transform'));
  AlignmentGeometry? get transformAlignment =>
      extractNativeValue<AlignmentGeometry>(prop('transform-alignment'));
  Clip get clipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.none;

  // DropdownButton properties
  FocusNode? get dropdownFocusNode =>
      extractNativeValue<FocusNode>(prop('focus-node'));
  bool get dropdownAutofocus => extractBool(prop('autofocus')) ?? false;

  // ListView properties
  Axis get listViewScrollDirection =>
      extractNativeValue<Axis>(prop('scroll-direction')) ?? Axis.vertical;
  bool get listViewReverse => extractBool(prop('reverse')) ?? false;
  ScrollController? get listViewController =>
      extractNativeValue<ScrollController>(prop('controller'));
  bool get listViewPrimary => extractBool(prop('primary')) ?? false;
  ScrollPhysics? get listViewPhysics =>
      extractNativeValue<ScrollPhysics>(prop('physics'));
  bool get listViewShrinkWrap => extractBool(prop('shrink-wrap')) ?? false;
  EdgeInsetsGeometry? get listViewPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding'));
  double? get listViewItemExtent => extractDouble(prop('item-extent'));
  Widget? get listViewPrototypeItem =>
      extractNativeValue<Widget>(prop('prototype-item'));
  bool get listViewAddAutomaticKeepAlives =>
      extractBool(prop('add-automatic-keep-alives')) ?? true;
  bool get listViewAddRepaintBoundaries =>
      extractBool(prop('add-repaint-boundaries')) ?? true;
  bool get listViewAddSemanticIndexes =>
      extractBool(prop('add-semantic-indexes')) ?? true;
  double? get listViewCacheExtent => extractDouble(prop('cache-extent'));
  List<Widget>? get listViewChildren => extractChildren(prop('children'));
  int? get listViewSemanticChildCount =>
      extractInt(prop('semantic-child-count'));
  Clip get listViewClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.hardEdge;

  // GridView properties
  Axis get gridViewScrollDirection =>
      extractNativeValue<Axis>(prop('scroll-direction')) ?? Axis.vertical;
  bool get gridViewReverse => extractBool(prop('reverse')) ?? false;
  ScrollController? get gridViewController =>
      extractNativeValue<ScrollController>(prop('controller'));
  bool get gridViewPrimary => extractBool(prop('primary')) ?? false;
  ScrollPhysics? get gridViewPhysics =>
      extractNativeValue<ScrollPhysics>(prop('physics'));
  bool get gridViewShrinkWrap => extractBool(prop('shrink-wrap')) ?? false;
  EdgeInsetsGeometry? get gridViewPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding'));
  SliverGridDelegate? get gridViewGridDelegate =>
      extractNativeValue<SliverGridDelegate>(prop('grid-delegate'));
  bool get gridViewAddAutomaticKeepAlives =>
      extractBool(prop('add-automatic-keep-alives')) ?? true;
  bool get gridViewAddRepaintBoundaries =>
      extractBool(prop('add-repaint-boundaries')) ?? true;
  bool get gridViewAddSemanticIndexes =>
      extractBool(prop('add-semantic-indexes')) ?? true;
  double? get gridViewCacheExtent => extractDouble(prop('cache-extent'));
  List<Widget>? get gridViewChildren => extractChildren(prop('children'));
  int? get gridViewSemanticChildCount =>
      extractInt(prop('semantic-child-count'));
  Clip get gridViewClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.hardEdge;

  // SingleChildScrollView properties
  Axis get singleChildScrollViewScrollDirection =>
      extractNativeValue<Axis>(prop('scroll-direction')) ?? Axis.vertical;
  bool get singleChildScrollViewReverse =>
      extractBool(prop('reverse')) ?? false;
  EdgeInsetsGeometry? get singleChildScrollViewPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding'));
  bool get singleChildScrollViewPrimary =>
      extractBool(prop('primary')) ?? false;
  ScrollPhysics? get singleChildScrollViewPhysics =>
      extractNativeValue<ScrollPhysics>(prop('physics'));
  ScrollController? get singleChildScrollViewController =>
      extractNativeValue<ScrollController>(prop('controller'));
  DragStartBehavior get singleChildScrollViewDragStartBehavior =>
      extractNativeValue<DragStartBehavior>(prop('drag-start-behavior')) ??
      DragStartBehavior.start;
  Clip get singleChildScrollViewClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.hardEdge;
  String? get singleChildScrollViewRestorationId =>
      extractString(prop('restoration-id'));
  ScrollViewKeyboardDismissBehavior
  get singleChildScrollViewKeyboardDismissBehavior =>
      extractNativeValue<ScrollViewKeyboardDismissBehavior>(
        prop('keyboard-dismiss-behavior'),
      ) ??
      ScrollViewKeyboardDismissBehavior.manual;
  Widget? get singleChildScrollViewChild =>
      extractNativeValue<Widget>(prop('child'));

  // CustomScrollView properties
  Axis get customScrollViewScrollDirection =>
      extractNativeValue<Axis>(prop('scroll-direction')) ?? Axis.vertical;
  bool get customScrollViewReverse => extractBool(prop('reverse')) ?? false;
  ScrollController? get customScrollViewController =>
      extractNativeValue<ScrollController>(prop('controller'));
  bool get customScrollViewPrimary => extractBool(prop('primary')) ?? false;
  ScrollPhysics? get customScrollViewPhysics =>
      extractNativeValue<ScrollPhysics>(prop('physics'));
  bool get customScrollViewShrinkWrap =>
      extractBool(prop('shrink-wrap')) ?? false;
  Key? get customScrollViewCenter => extractNativeValue<Key>(prop('center'));
  double get customScrollViewAnchor => extractDouble(prop('anchor')) ?? 0.0;
  double? get customScrollViewCacheExtent =>
      extractDouble(prop('cache-extent'));
  List<Widget>? get customScrollViewSlivers => extractChildren(prop('slivers'));
  int? get customScrollViewSemanticChildCount =>
      extractInt(prop('semantic-child-count'));
  DragStartBehavior get customScrollViewDragStartBehavior =>
      extractNativeValue<DragStartBehavior>(prop('drag-start-behavior')) ??
      DragStartBehavior.start;
  ScrollViewKeyboardDismissBehavior
  get customScrollViewKeyboardDismissBehavior =>
      extractNativeValue<ScrollViewKeyboardDismissBehavior>(
        prop('keyboard-dismiss-behavior'),
      ) ??
      ScrollViewKeyboardDismissBehavior.manual;
  String? get customScrollViewRestorationId =>
      extractString(prop('restoration-id'));
  Clip get customScrollViewClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.hardEdge;

  // SliverList properties
  SliverChildDelegate? get sliverListDelegate =>
      extractNativeValue<SliverChildDelegate>(prop('delegate'));

  // SliverGrid properties
  SliverChildDelegate? get sliverGridDelegate =>
      extractNativeValue<SliverChildDelegate>(prop('delegate'));
  SliverGridDelegate? get sliverGridGridDelegate =>
      extractNativeValue<SliverGridDelegate>(prop('grid-delegate'));
}
