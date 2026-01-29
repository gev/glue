import 'package:flutter/cupertino.dart';
import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart' show SearchController;
import 'package:flutter/services.dart';
import 'package:glue/runtime.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// Default hero tag for navigation bars
const _defaultHeroTag = '<default-hero-tag>';

/// Properties wrapper with lazy getters
/// Provides clean API for accessing widget properties without pre-computing everything
class CupertinoProperties extends CoreProperties {
  CupertinoProperties(super.props);
  CupertinoProperties.empty() : super.empty();

  // Button properties
  String? get label => extractString(prop('label'));
  VoidCallback? onPress(Runtime runtime) =>
      extractVoidCallback(prop('on-press'), runtime);
  VoidCallback? onLongPress(Runtime runtime) =>
      extractVoidCallback(prop('on-long-press'), runtime);
  ValueChanged<bool>? get onHover => extractNativeValue(prop('on-hover'));
  ValueChanged<bool>? get onFocusChange =>
      extractNativeValue(prop('on-focus-change'));
  FocusNode? get focusNode => extractNativeValue(prop('focus-node'));
  bool get autofocus => extractBool(prop('autofocus')) ?? false;
  Clip get buttonClipBehavior =>
      extractNativeValue(prop('clip-behavior')) ?? Clip.none;

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

  // FlutterLogo properties
  FlutterLogoStyle? get flutterLogoStyle =>
      extractNativeValue<FlutterLogoStyle>(prop('style'));
  Duration? get duration => extractNativeValue<Duration>(prop('duration'));
  Curve? get curve => extractNativeValue<Curve>(prop('curve'));

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

  // AppBar properties
  Widget? get title => extractNativeValue<Widget>(prop('title'));
  List<Widget>? get actions => extractChildren(prop('actions'));
  Color? get foregroundColor => extractColor(prop('foreground-color'));
  Color? get shadowColor => extractColor(prop('shadow-color'));
  Color? get surfaceTintColor => extractColor(prop('surface-tint-color'));
  bool? get centerTitle => extractBool(prop('center-title'));
  double? get titleSpacing => extractDouble(prop('title-spacing'));
  double? get toolbarOpacity => extractDouble(prop('toolbar-opacity'));
  double? get bottomOpacity => extractDouble(prop('bottom-opacity'));
  double? get toolbarHeight => extractDouble(prop('toolbar-height'));
  double? get leadingWidth => extractDouble(prop('leading-width'));
  bool? get primary => extractBool(prop('primary'));
  bool? get excludeHeaderSemantics =>
      extractBool(prop('exclude-header-semantics'));

  // Advanced AppBar properties
  bool? get automaticallyImplyLeading =>
      extractBool(prop('automatically-imply-leading'));
  bool? get automaticallyImplyActions =>
      extractBool(prop('automatically-imply-actions'));
  Widget? get flexibleSpace =>
      extractNativeValue<Widget>(prop('flexible-space'));
  PreferredSizeWidget? get bottomAppBar =>
      extractNativeValue<PreferredSizeWidget>(prop('bottom'));
  double? get scrolledUnderElevation =>
      extractDouble(prop('scrolled-under-elevation'));
  ScrollNotificationPredicate? get notificationPredicate =>
      extractNativeValue<ScrollNotificationPredicate>(
        prop('notification-predicate'),
      );
  ShapeBorder? get shape => extractNativeValue<ShapeBorder>(prop('shape'));
  IconThemeData? get iconTheme =>
      extractNativeValue<IconThemeData>(prop('icon-theme'));
  IconThemeData? get actionsIconTheme =>
      extractNativeValue<IconThemeData>(prop('actions-icon-theme'));
  TextStyle? get titleTextStyle =>
      extractNativeValue<TextStyle>(prop('title-text-style'));
  TextStyle? get toolbarTextStyle =>
      extractNativeValue<TextStyle>(prop('toolbar-text-style'));
  SystemUiOverlayStyle? get systemOverlayStyle =>
      extractNativeValue<SystemUiOverlayStyle>(prop('system-overlay-style'));
  bool? get forceMaterialTransparency =>
      extractBool(prop('force-material-transparency'));
  bool? get useDefaultSemanticsOrder =>
      extractBool(prop('use-default-semantics-order'));
  EdgeInsetsGeometry? get actionsPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('actions-padding'));
  bool? get animateColor => extractBool(prop('animate-color'));

  // Scaffold properties
  PreferredSizeWidget? get appBar =>
      extractNativeValue<PreferredSizeWidget>(prop('app-bar'));
  Widget? get body => extractNativeValue<Widget>(prop('body'));
  Widget? get floatingActionButton =>
      extractNativeValue<Widget>(prop('floating-action-button'));
  Widget? get floatingActionButtonAnimator =>
      extractNativeValue<Widget>(prop('floating-action-button-animator'));
  List<Widget>? get persistentFooterButtons =>
      extractChildren(prop('persistent-footer-buttons'));
  Widget? get drawer => extractNativeValue<Widget>(prop('drawer'));
  Widget? get endDrawer => extractNativeValue<Widget>(prop('end-drawer'));
  Widget? get bottomNavigationBar =>
      extractNativeValue<Widget>(prop('bottom-navigation-bar'));
  Widget? get bottomSheet => extractNativeValue<Widget>(prop('bottom-sheet'));
  bool? get resizeToAvoidBottomInset =>
      extractBool(prop('resize-to-avoid-bottom-inset'));
  bool? get extendBody => extractBool(prop('extend-body'));
  bool? get extendBodyBehindAppBar =>
      extractBool(prop('extend-body-behind-app-bar'));
  Color? get drawerScrimColor => extractColor(prop('drawer-scrim-color'));
  double? get drawerEdgeDragWidth =>
      extractDouble(prop('drawer-edge-drag-width'));
  bool? get drawerEnableOpenDragGesture =>
      extractBool(prop('drawer-enable-open-drag-gesture'));
  bool? get endDrawerEnableOpenDragGesture =>
      extractBool(prop('end-drawer-enable-open-drag-gesture'));
  String? get restorationId => extractString(prop('restoration-id'));

  // Card properties
  bool? get borderOnForeground => extractBool(prop('border-on-foreground'));
  bool? get semanticContainer => extractBool(prop('semantic-container'));

  // ListTile properties
  Widget? get subtitle => extractNativeValue<Widget>(prop('subtitle'));
  Widget? get trailing => extractNativeValue<Widget>(prop('trailing'));
  bool? get isThreeLine => extractBool(prop('is-three-line'));
  bool? get dense => extractBool(prop('dense'));
  Color? get selectedColor => extractColor(prop('selected-color'));
  Color? get iconColor => extractColor(prop('icon-color'));
  Color? get textColor => extractColor(prop('text-color'));
  TextStyle? get listTileTitleTextStyle =>
      extractNativeValue<TextStyle>(prop('title-text-style'));
  TextStyle? get subtitleTextStyle =>
      extractNativeValue<TextStyle>(prop('subtitle-text-style'));
  TextStyle? get leadingAndTrailingTextStyle =>
      extractNativeValue<TextStyle>(prop('leading-and-trailing-text-style'));
  EdgeInsetsGeometry? get contentPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('content-padding'));
  bool? get enabled => extractBool(prop('enabled'));
  GestureTapCallback? get onTileTap =>
      extractNativeValue<GestureTapCallback>(prop('on-press'));
  GestureLongPressCallback? get onTileLongPress =>
      extractNativeValue<GestureLongPressCallback>(prop('on-long-press'));
  MouseCursor? get mouseCursor =>
      extractNativeValue<MouseCursor>(prop('mouse-cursor'));
  bool? get selected => extractBool(prop('selected'));
  Color? get focusColor => extractColor(prop('focus-color'));
  Color? get hoverColor => extractColor(prop('hover-color'));
  Color? get splashColor => extractColor(prop('splash-color'));
  Color? get tileColor => extractColor(prop('tile-color'));
  Color? get selectedTileColor => extractColor(prop('selected-tile-color'));
  bool? get enableFeedback => extractBool(prop('enable-feedback'));
  double? get horizontalTitleGap => extractDouble(prop('horizontal-title-gap'));
  double? get minVerticalPadding => extractDouble(prop('min-vertical-padding'));
  double? get minLeadingWidth => extractDouble(prop('min-leading-width'));
  double? get minTileHeight => extractDouble(prop('min-tile-height'));

  // SnackBar properties
  Widget? get snackBarContent => extractNativeValue<Widget>(prop('content'));
  Duration? get snackBarDuration =>
      extractNativeValue<Duration>(prop('duration'));
  Animation<double>? get snackBarAnimation =>
      extractNativeValue<Animation<double>>(prop('animation'));
  VoidCallback? onVisible(Runtime runtime) =>
      extractVoidCallback(prop('on-visible'), runtime);
  DismissDirection? get dismissDirection =>
      extractNativeValue<DismissDirection>(prop('dismiss-direction'));

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

  // FloatingActionButton properties
  String? get tooltip => extractString(prop('tooltip'));
  Object? get heroTag => extractNativeValue<Object>(prop('hero-tag'));
  double? get focusElevation => extractDouble(prop('focus-elevation'));
  double? get hoverElevation => extractDouble(prop('hover-elevation'));
  double? get highlightElevation => extractDouble(prop('highlight-elevation'));
  double? get disabledElevation => extractDouble(prop('disabled-elevation'));
  bool? get mini => extractBool(prop('mini'));
  bool get isExtended => extractBool(prop('is-extended')) ?? false;
  bool? get fabEnableFeedback => extractBool(prop('enable-feedback'));
  Alignment? get fabAlignment =>
      extractNativeValue<Alignment>(prop('alignment'));
  Offset? get fabOffset => extractNativeValue<Offset>(prop('offset'));

  // IconButton properties
  double get iconButtonIconSize => extractDouble(prop('icon-size')) ?? 24.0;
  EdgeInsetsGeometry get iconButtonPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding')) ??
      const EdgeInsets.all(8.0);
  AlignmentGeometry get iconButtonAlignment =>
      extractNativeValue<AlignmentGeometry>(prop('alignment')) ??
      Alignment.center;
  double? get splashRadius => extractDouble(prop('splash-radius'));
  bool? get iconButtonMini => extractBool(prop('mini'));
  Color? get highlightColor => extractColor(prop('highlight-color'));
  Color? get disabledColor => extractColor(prop('disabled-color'));

  // Checkbox properties
  bool? get checkboxValue => extractBool(prop('value'));
  bool get tristate => extractBool(prop('tristate')) ?? false;
  ValueChanged<bool?>? get onCheckboxChanged =>
      extractNativeValue<ValueChanged<bool?>>(prop('on-changed'));
  Color? get activeColor => extractColor(prop('active-color'));
  WidgetStateProperty<Color?>? get fillColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('fill-color'));
  Color? get checkColor => extractColor(prop('check-color'));
  OutlinedBorder? get checkboxShape =>
      extractNativeValue<OutlinedBorder>(prop('shape'));
  BorderSide? get checkboxSide => extractNativeValue<BorderSide>(prop('side'));
  bool get isError => extractBool(prop('is-error')) ?? false;
  String? get checkboxSemanticLabel => extractString(prop('semantic-label'));
  WidgetStateProperty<Color?>? get overlayColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('overlay-color'));

  // Switch properties
  bool get switchValue => extractBool(prop('value')) ?? false;
  ValueChanged<bool>? get onSwitchChanged =>
      extractNativeValue<ValueChanged<bool>>(prop('on-changed'));
  Color? get activeThumbColor => extractColor(prop('active-thumb-color'));
  Color? get activeTrackColor => extractColor(prop('active-track-color'));
  Color? get inactiveThumbColor => extractColor(prop('inactive-thumb-color'));
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

  // LinearProgressIndicator properties
  double? get progressValue => extractDouble(prop('value'));
  double? get progressMinHeight => extractDouble(prop('min-height'));
  String? get progressSemanticsLabel => extractString(prop('semantics-label'));
  String? get progressSemanticsValue => extractString(prop('semantics-value'));
  Animation<Color>? get valueColor =>
      extractNativeValue<Animation<Color>>(prop('value-color'));

  // Badge properties
  Widget? get badgeLabel => extractNativeValue<Widget>(prop('label'));
  bool? get isLabelVisible => extractBool(prop('is-label-visible'));
  bool? get largeSize => extractBool(prop('large-size'));
  Offset? get badgeOffset => extractNativeValue<Offset>(prop('offset'));
  bool? get showBadge => extractBool(prop('show-badge'));

  // Divider properties
  double? get dividerHeight => extractDouble(prop('height'));
  double? get dividerThickness => extractDouble(prop('thickness'));
  double? get dividerIndent => extractDouble(prop('indent'));
  double? get dividerEndIndent => extractDouble(prop('end-indent'));
  BorderRadiusGeometry? get dividerRadius =>
      extractNativeValue<BorderRadiusGeometry>(prop('radius'));

  // AlertDialog properties
  Widget? get alertDialogIcon => extractNativeValue<Widget>(prop('icon'));
  EdgeInsetsGeometry? get alertDialogIconPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('icon-padding'));
  Color? get alertDialogIconColor => extractColor(prop('icon-color'));
  EdgeInsetsGeometry? get alertDialogTitlePadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('title-padding'));
  TextStyle? get alertDialogTitleTextStyle =>
      extractNativeValue<TextStyle>(prop('title-text-style'));
  EdgeInsetsGeometry? get alertDialogContentPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('content-padding'));
  TextStyle? get alertDialogContentTextStyle =>
      extractNativeValue<TextStyle>(prop('content-text-style'));
  List<Widget>? get alertDialogActions => extractChildren(prop('actions'));
  EdgeInsetsGeometry? get alertDialogActionsPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('actions-padding'));
  MainAxisAlignment? get alertDialogActionsAlignment =>
      extractNativeValue<MainAxisAlignment>(prop('actions-alignment'));
  OverflowBarAlignment? get alertDialogActionsOverflowAlignment =>
      extractNativeValue<OverflowBarAlignment>(
        prop('actions-overflow-alignment'),
      );
  VerticalDirection? get alertDialogActionsOverflowDirection =>
      extractNativeValue<VerticalDirection>(prop('actions-overflow-direction'));
  double? get alertDialogActionsOverflowButtonSpacing =>
      extractDouble(prop('actions-overflow-button-spacing'));
  EdgeInsetsGeometry? get alertDialogButtonPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('button-padding'));
  String? get alertDialogSemanticLabel => extractString(prop('semantic-label'));
  EdgeInsets? get alertDialogInsetPadding =>
      extractNativeValue<EdgeInsets>(prop('inset-padding'));
  bool get alertDialogScrollable => extractBool(prop('scrollable')) ?? false;

  // Chip properties
  Widget? get chipAvatar => extractNativeValue<Widget>(prop('avatar'));
  TextStyle? get chipLabelStyle =>
      extractNativeValue<TextStyle>(prop('label-style'));
  EdgeInsetsGeometry? get chipLabelPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('label-padding'));
  Widget? get chipDeleteIcon => extractNativeValue<Widget>(prop('delete-icon'));
  VoidCallback? chipOnDeleted(Runtime runtime) =>
      extractVoidCallback(prop('on-deleted'), runtime);
  Color? get chipDeleteIconColor => extractColor(prop('delete-icon-color'));
  String? get chipDeleteButtonTooltipMessage =>
      extractString(prop('delete-button-tooltip-message'));
  BorderSide? get chipSide => extractNativeValue<BorderSide>(prop('side'));
  OutlinedBorder? get chipShape =>
      extractNativeValue<OutlinedBorder>(prop('shape'));
  WidgetStateProperty<Color?>? get chipColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('color'));
  EdgeInsetsGeometry? get chipPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding'));
  BoxConstraints? get chipAvatarBoxConstraints =>
      extractNativeValue<BoxConstraints>(prop('avatar-box-constraints'));
  BoxConstraints? get chipDeleteIconBoxConstraints =>
      extractNativeValue<BoxConstraints>(prop('delete-icon-box-constraints'));

  // SegmentedButton properties
  Set<dynamic>? get segmentedSelected =>
      extractNativeValue<Set<dynamic>>(prop('selected'));
  List<Widget>? get segmentedSegments => extractChildren(prop('segments'));
  ValueChanged<Set<dynamic>>? get onSegmentedSelectionChanged =>
      extractNativeValue<ValueChanged<Set<dynamic>>>(
        prop('on-selection-changed'),
      );
  Set<dynamic>? get multiSelectionEnabledFor =>
      extractNativeValue<Set<dynamic>>(prop('multi-selection-enabled-for'));
  bool? get showSelectedIcon => extractBool(prop('show-selected-icon'));
  Color? get segmentedBackgroundColor => extractColor(prop('background-color'));
  Color? get segmentedUnselectedColor => extractColor(prop('unselected-color'));
  Color? get segmentedSelectedColor => extractColor(prop('selected-color'));
  Color? get segmentedDisabledColor => extractColor(prop('disabled-color'));
  Color? get segmentedShadowColor => extractColor(prop('shadow-color'));
  Color? get segmentedSurfaceTintColor =>
      extractColor(prop('surface-tint-color'));
  double? get segmentedElevation => extractDouble(prop('elevation'));

  // BottomNavigationBar properties
  List<BottomNavigationBarItem>? get bottomNavigationBarItems =>
      extractNativeValue<List<BottomNavigationBarItem>>(prop('items'));
  ValueChanged<int>? get onBottomNavigationBarTap =>
      extractNativeValue<ValueChanged<int>>(prop('on-press'));
  int get bottomNavigationBarCurrentIndex =>
      extractInt(prop('current-index')) ?? 0;
  double get bottomNavigationBarElevation =>
      extractDouble(prop('elevation')) ?? 8.0;
  Color? get bottomNavigationBarFixedColor => extractColor(prop('fixed-color'));
  Color? get bottomNavigationBarBackgroundColor =>
      extractColor(prop('background-color'));
  double get bottomNavigationBarIconSize =>
      extractDouble(prop('icon-size')) ?? 24.0;
  Color? get bottomNavigationBarSelectedItemColor =>
      extractColor(prop('selected-item-color'));
  Color? get bottomNavigationBarUnselectedItemColor =>
      extractColor(prop('unselected-item-color'));
  IconThemeData? get bottomNavigationBarSelectedIconTheme =>
      extractNativeValue<IconThemeData>(prop('selected-icon-theme'));
  IconThemeData? get bottomNavigationBarUnselectedIconTheme =>
      extractNativeValue<IconThemeData>(prop('unselected-icon-theme'));
  TextStyle? get bottomNavigationBarSelectedLabelStyle =>
      extractNativeValue<TextStyle>(prop('selected-label-style'));
  TextStyle? get bottomNavigationBarUnselectedLabelStyle =>
      extractNativeValue<TextStyle>(prop('unselected-label-style'));
  double get bottomNavigationBarSelectedFontSize =>
      extractDouble(prop('selected-font-size')) ?? 14.0;
  double get bottomNavigationBarUnselectedFontSize =>
      extractDouble(prop('unselected-font-size')) ?? 12.0;
  bool get bottomNavigationBarShowSelectedLabels =>
      extractBool(prop('show-selected-labels')) ?? true;
  bool get bottomNavigationBarShowUnselectedLabels =>
      extractBool(prop('show-unselected-labels')) ?? true;
  bool get bottomNavigationBarEnableFeedback =>
      extractBool(prop('enable-feedback')) ?? true;

  // Drawer properties
  double get drawerWidth => extractDouble(prop('width')) ?? 304.0;
  Color? get drawerBackgroundColor => extractColor(prop('background-color'));
  Color? get drawerScrimColorProperty => extractColor(prop('scrim-color'));
  double get drawerElevation => extractDouble(prop('elevation')) ?? 16.0;
  Color? get drawerShadowColor => extractColor(prop('shadow-color'));
  Color? get drawerSurfaceTintColor => extractColor(prop('surface-tint-color'));
  ShapeBorder? get drawerShape =>
      extractNativeValue<ShapeBorder>(prop('shape'));
  double get drawerSemanticLabel =>
      extractDouble(prop('semantic-label')) ?? 0.0;
  Clip get drawerClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.hardEdge;

  // Radio properties
  dynamic get radioValue => extractNativeValue<dynamic>(prop('value'));
  dynamic get radioGroupValue =>
      extractNativeValue<dynamic>(prop('group-value'));
  ValueChanged<dynamic>? get onRadioChanged =>
      extractNativeValue<ValueChanged<dynamic>>(prop('on-changed'));
  bool get toggleable => extractBool(prop('toggleable')) ?? false;
  WidgetStateProperty<Color?>? get radioFillColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('fill-color'));

  // Slider properties
  double get sliderValue => extractDouble(prop('value')) ?? 0.0;
  double? get sliderSecondaryTrackValue =>
      extractDouble(prop('secondary-track-value'));
  ValueChanged<double>? get onSliderChanged =>
      extractNativeValue<ValueChanged<double>>(prop('on-changed'));
  ValueChanged<double>? get onSliderChangeStart =>
      extractNativeValue<ValueChanged<double>>(prop('on-change-start'));
  ValueChanged<double>? get onSliderChangeEnd =>
      extractNativeValue<ValueChanged<double>>(prop('on-change-end'));
  double get sliderMin => extractDouble(prop('min')) ?? 0.0;
  double get sliderMax => extractDouble(prop('max')) ?? 1.0;
  int? get sliderDivisions => extractInt(prop('divisions'));
  String? get sliderLabel => extractString(prop('label'));
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

  // DatePickerDialog properties
  DateTime? get datePickerInitialDate =>
      extractNativeValue<DateTime>(prop('initial-date'));
  DateTime? get datePickerFirstDate =>
      extractNativeValue<DateTime>(prop('first-date'));
  DateTime? get datePickerLastDate =>
      extractNativeValue<DateTime>(prop('last-date'));
  DateTime? get datePickerCurrentDate =>
      extractNativeValue<DateTime>(prop('current-date'));
  SelectableDayPredicate? get datePickerSelectableDayPredicate =>
      extractNativeValue<SelectableDayPredicate>(
        prop('selectable-day-predicate'),
      );
  String? get datePickerCancelText => extractString(prop('cancel-text'));
  String? get datePickerConfirmText => extractString(prop('confirm-text'));
  String? get datePickerHelpText => extractString(prop('help-text'));
  String? get datePickerErrorFormatText =>
      extractString(prop('error-format-text'));
  String? get datePickerErrorInvalidText =>
      extractString(prop('error-invalid-text'));
  String? get datePickerFieldHintText => extractString(prop('field-hint-text'));
  String? get datePickerFieldLabelText =>
      extractString(prop('field-label-text'));
  TextInputType? get datePickerKeyboardType =>
      extractNativeValue<TextInputType>(prop('keyboard-type'));
  String? get datePickerRestorationId => extractString(prop('restoration-id'));
  Icon? get datePickerSwitchToInputEntryModeIcon =>
      extractNativeValue<Icon>(prop('switch-to-input-entry-mode-icon'));
  Icon? get datePickerSwitchToCalendarEntryModeIcon =>
      extractNativeValue<Icon>(prop('switch-to-calendar-entry-mode-icon'));
  EdgeInsets get datePickerInsetPadding =>
      extractNativeValue<EdgeInsets>(prop('inset-padding')) ??
      const EdgeInsets.symmetric(horizontal: 16.0, vertical: 24.0);

  // TimePickerDialog properties
  String? get timePickerCancelText => extractString(prop('cancel-text'));
  String? get timePickerConfirmText => extractString(prop('confirm-text'));
  String? get timePickerHelpText => extractString(prop('help-text'));
  String? get timePickerErrorInvalidText =>
      extractString(prop('error-invalid-text'));
  String? get timePickerHourLabelText => extractString(prop('hour-label-text'));
  String? get timePickerMinuteLabelText =>
      extractString(prop('minute-label-text'));
  String? get timePickerRestorationId => extractString(prop('restoration-id'));
  Orientation? get timePickerOrientation =>
      extractNativeValue<Orientation>(prop('orientation'));
  Icon? get timePickerSwitchToInputEntryModeIcon =>
      extractNativeValue<Icon>(prop('switch-to-input-entry-mode-icon'));
  Icon? get timePickerSwitchToTimerEntryModeIcon =>
      extractNativeValue<Icon>(prop('switch-to-timer-entry-mode-icon'));
  bool get timePickerEmptyInitialInput =>
      extractBool(prop('empty-initial-input')) ?? false;

  // NavigationBar properties
  Duration? get navigationBarAnimationDuration =>
      extractNativeValue<Duration>(prop('animation-duration'));
  int get navigationBarSelectedIndex => extractInt(prop('selected-index')) ?? 0;
  List<Widget>? get navigationBarDestinations =>
      extractChildren(prop('destinations'));
  ValueChanged<int>? get navigationBarOnDestinationSelected =>
      extractNativeValue<ValueChanged<int>>(prop('on-destination-selected'));
  Color? get navigationBarBackgroundColor =>
      extractColor(prop('background-color'));
  double? get navigationBarElevation => extractDouble(prop('elevation'));
  Color? get navigationBarShadowColor => extractColor(prop('shadow-color'));
  Color? get navigationBarSurfaceTintColor =>
      extractColor(prop('surface-tint-color'));
  Color? get navigationBarIndicatorColor =>
      extractColor(prop('indicator-color'));
  ShapeBorder? get navigationBarIndicatorShape =>
      extractNativeValue<ShapeBorder>(prop('indicator-shape'));
  double? get navigationBarHeight => extractDouble(prop('height'));
  WidgetStateProperty<Color?>? get navigationBarOverlayColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('overlay-color'));
  WidgetStateProperty<TextStyle?>? get navigationBarLabelTextStyle =>
      extractNativeValue<WidgetStateProperty<TextStyle?>>(
        prop('label-text-style'),
      );
  EdgeInsetsGeometry? get navigationBarLabelPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('label-padding'));
  bool get navigationBarMaintainBottomViewPadding =>
      extractBool(prop('maintain-bottom-view-padding')) ?? false;

  // TabBar properties
  List<Widget>? get tabBarTabs => extractChildren(prop('tabs'));
  bool get tabBarIsScrollable => extractBool(prop('is-scrollable')) ?? false;
  EdgeInsetsGeometry? get tabBarPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding'));
  Color? get tabBarIndicatorColor => extractColor(prop('indicator-color'));
  bool get tabBarAutomaticIndicatorColorAdjustment =>
      extractBool(prop('automatic-indicator-color-adjustment')) ?? true;
  double get tabBarIndicatorWeight =>
      extractDouble(prop('indicator-weight')) ?? 2.0;
  EdgeInsetsGeometry get tabBarIndicatorPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('indicator-padding')) ??
      EdgeInsets.zero;
  Decoration? get tabBarIndicator =>
      extractNativeValue<Decoration>(prop('indicator'));
  Color? get tabBarDividerColor => extractColor(prop('divider-color'));
  double? get tabBarDividerHeight => extractDouble(prop('divider-height'));
  Color? get tabBarLabelColor => extractColor(prop('label-color'));
  TextStyle? get tabBarLabelStyle =>
      extractNativeValue<TextStyle>(prop('label-style'));
  EdgeInsetsGeometry? get tabBarLabelPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('label-padding'));
  Color? get tabBarUnselectedLabelColor =>
      extractColor(prop('unselected-label-color'));
  TextStyle? get tabBarUnselectedLabelStyle =>
      extractNativeValue<TextStyle>(prop('unselected-label-style'));
  DragStartBehavior get tabBarDragStartBehavior =>
      extractNativeValue<DragStartBehavior>(prop('drag-start-behavior')) ??
      DragStartBehavior.start;
  WidgetStateProperty<Color?>? get tabBarOverlayColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('overlay-color'));
  MouseCursor? get tabBarMouseCursor =>
      extractNativeValue<MouseCursor>(prop('mouse-cursor'));
  bool? get tabBarEnableFeedback => extractBool(prop('enable-feedback'));
  ValueChanged<int>? get tabBarOnTap =>
      extractNativeValue<ValueChanged<int>>(prop('on-press'));
  ScrollPhysics? get tabBarPhysics =>
      extractNativeValue<ScrollPhysics>(prop('physics'));

  // MenuAnchor properties
  MenuController? get menuAnchorController =>
      extractNativeValue<MenuController>(prop('controller'));
  Offset get menuAnchorAlignmentOffset =>
      extractNativeValue<Offset>(prop('alignment-offset')) ?? Offset.zero;
  EdgeInsetsGeometry? get menuAnchorReservedPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('reserved-padding'));
  LayerLink? get menuAnchorLayerLink =>
      extractNativeValue<LayerLink>(prop('layer-link'));
  Clip get menuAnchorClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.hardEdge;
  bool get menuAnchorConsumeOutsideTap =>
      extractBool(prop('consume-outside-tap')) ?? false;
  VoidCallback? get menuAnchorOnOpen =>
      extractNativeValue<VoidCallback>(prop('on-open'));
  VoidCallback? get menuAnchorOnClose =>
      extractNativeValue<VoidCallback>(prop('on-close'));
  bool get menuAnchorCrossAxisUnconstrained =>
      extractBool(prop('cross-axis-unconstrained')) ?? true;
  bool get menuAnchorUseRootOverlay =>
      extractBool(prop('use-root-overlay')) ?? false;
  List<Widget>? get menuAnchorMenuChildren =>
      extractChildren(prop('menu-children'));

  // ExpansionTile properties
  Widget? get expansionTileLeading =>
      extractNativeValue<Widget>(prop('leading'));
  Widget? get expansionTileTitle => extractNativeValue<Widget>(prop('title'));
  Widget? get expansionTileSubtitle =>
      extractNativeValue<Widget>(prop('subtitle'));
  Widget? get expansionTileTrailing =>
      extractNativeValue<Widget>(prop('trailing'));
  List<Widget>? get expansionTileChildren => extractChildren(prop('children'));
  bool get expansionTileInitiallyExpanded =>
      extractBool(prop('initially-expanded')) ?? false;
  bool get expansionTileMaintainState =>
      extractBool(prop('maintain-state')) ?? false;
  EdgeInsetsGeometry get expansionTileTilePadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('tile-padding')) ??
      const EdgeInsets.symmetric(horizontal: 16.0, vertical: 8.0);
  Alignment get expansionTileExpandedAlignment =>
      extractNativeValue<Alignment>(prop('expanded-alignment')) ??
      Alignment.centerLeft;
  CrossAxisAlignment get expansionTileExpandedCrossAxisAlignment =>
      extractNativeValue<CrossAxisAlignment>(
        prop('expanded-cross-axis-align'),
      ) ??
      CrossAxisAlignment.center;
  EdgeInsetsGeometry get expansionTileChildrenPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('children-padding')) ??
      const EdgeInsets.symmetric(vertical: 8.0);
  Color? get expansionTileBackgroundColor =>
      extractColor(prop('background-color'));
  Color? get expansionTileCollapsedBackgroundColor =>
      extractColor(prop('collapsed-background-color'));
  Color? get expansionTileTextColor => extractColor(prop('text-color'));
  Color? get expansionTileCollapsedTextColor =>
      extractColor(prop('collapsed-text-color'));
  Color? get expansionTileIconColor => extractColor(prop('icon-color'));
  Color? get expansionTileCollapsedIconColor =>
      extractColor(prop('collapsed-icon-color'));
  ValueChanged<bool>? get expansionTileOnExpansionChanged =>
      extractNativeValue<ValueChanged<bool>>(prop('on-expansion-changed'));

  // DataTable properties
  int? get dataTableSortColumnIndex => extractInt(prop('sort-column-index'));
  bool get dataTableSortAscending =>
      extractBool(prop('sort-ascending')) ?? true;
  ValueSetter<bool?>? get dataTableOnSelectAll =>
      extractNativeValue<ValueSetter<bool?>>(prop('on-select-all'));
  WidgetStateProperty<Color?>? get dataTableDataRowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('data-row-color'));
  double get dataTableDataRowHeight =>
      extractDouble(prop('data-row-height')) ?? 48.0;
  TextStyle? get dataTableDataTextStyle =>
      extractNativeValue<TextStyle>(prop('data-text-style'));
  WidgetStateProperty<Color?>? get dataTableHeadingRowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        prop('heading-row-color'),
      );
  double get dataTableHeadingRowHeight =>
      extractDouble(prop('heading-row-height')) ?? 56.0;
  TextStyle? get dataTableHeadingTextStyle =>
      extractNativeValue<TextStyle>(prop('heading-text-style'));
  double get dataTableHorizontalMargin =>
      extractDouble(prop('horizontal-margin')) ?? 24.0;
  double get dataTableColumnSpacing =>
      extractDouble(prop('column-spacing')) ?? 56.0;
  bool get dataTableShowCheckboxColumn =>
      extractBool(prop('show-checkbox-column')) ?? true;
  bool get dataTableShowBottomBorder =>
      extractBool(prop('show-bottom-border')) ?? true;
  double get dataTableDividerThickness =>
      extractDouble(prop('divider-thickness')) ?? 1.0;
  double get dataTableCheckboxHorizontalMargin =>
      extractDouble(prop('checkbox-horizontal-margin')) ?? 24.0;
  TableBorder? get dataTableBorder =>
      extractNativeValue<TableBorder>(prop('border'));
  Clip get dataTableClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.none;

  // Tooltip properties
  String get tooltipMessage => extractString(prop('message')) ?? '';
  double get tooltipHeight => extractDouble(prop('height')) ?? 32.0;
  EdgeInsetsGeometry get tooltipPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding')) ??
      const EdgeInsets.symmetric(horizontal: 16.0);
  EdgeInsetsGeometry? get tooltipMargin =>
      extractNativeValue<EdgeInsetsGeometry>(prop('margin'));
  double get tooltipVerticalOffset =>
      extractDouble(prop('vertical-offset')) ?? 24.0;
  bool get tooltipPreferBelow => extractBool(prop('prefer-below')) ?? true;
  bool get tooltipExcludeFromSemantics =>
      extractBool(prop('exclude-from-semantics')) ?? false;
  Decoration? get tooltipDecoration =>
      extractNativeValue<Decoration>(prop('decoration'));
  TextStyle? get tooltipTextStyle =>
      extractNativeValue<TextStyle>(prop('text-style'));
  TextAlign get tooltipTextAlign =>
      extractNativeValue<TextAlign>(prop('text-align')) ?? TextAlign.start;
  Duration get tooltipWaitDuration =>
      extractNativeValue<Duration>(prop('wait-duration')) ??
      const Duration(milliseconds: 0);
  Duration get tooltipShowDuration =>
      extractNativeValue<Duration>(prop('show-duration')) ??
      const Duration(milliseconds: 1500);
  bool get tooltipEnableFeedback =>
      extractBool(prop('enable-feedback')) ?? true;
  InlineSpan? get tooltipRichMessage =>
      extractNativeValue<InlineSpan>(prop('rich-message'));

  // PopupMenuButton properties
  Object? get popupMenuInitialValue =>
      extractNativeValue<Object>(prop('initial-value'));
  String? get popupMenuTooltip => extractString(prop('tooltip'));
  double get popupMenuElevation => extractDouble(prop('elevation')) ?? 8.0;
  EdgeInsetsGeometry get popupMenuPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding')) ??
      const EdgeInsets.all(8.0);
  Widget? get popupMenuChild => extractNativeValue<Widget>(prop('child'));
  double? get popupMenuSplashRadius => extractDouble(prop('splash-radius'));
  Widget? get popupMenuIcon => extractNativeValue<Widget>(prop('icon'));
  double get popupMenuIconSize => extractDouble(prop('icon-size')) ?? 24.0;
  Offset get popupMenuOffset =>
      extractNativeValue<Offset>(prop('offset')) ?? Offset.zero;
  bool get popupMenuEnabled => extractBool(prop('enabled')) ?? true;
  ShapeBorder? get popupMenuShape =>
      extractNativeValue<ShapeBorder>(prop('shape'));
  Color? get popupMenuColor => extractColor(prop('color'));
  bool get popupMenuEnableFeedback =>
      extractBool(prop('enable-feedback')) ?? true;
  BoxConstraints? get popupMenuConstraints =>
      extractNativeValue<BoxConstraints>(prop('constraints'));

  // DropdownButton properties
  Object? get dropdownValue => extractNativeValue<Object>(prop('value'));
  Widget? get dropdownHint => extractNativeValue<Widget>(prop('hint'));
  Widget? get dropdownDisabledHint =>
      extractNativeValue<Widget>(prop('disabled-hint'));
  ValueChanged<Object?>? get dropdownOnChanged =>
      extractNativeValue<ValueChanged<Object?>>(prop('on-changed'));
  GestureTapCallback? get dropdownOnTap =>
      extractNativeValue<GestureTapCallback>(prop('on-press'));
  int get dropdownElevation => extractInt(prop('elevation')) ?? 8;
  TextStyle? get dropdownStyle => extractNativeValue<TextStyle>(prop('style'));
  Widget? get dropdownUnderline =>
      extractNativeValue<Widget>(prop('underline'));
  Widget? get dropdownIcon => extractNativeValue<Widget>(prop('icon'));
  Color? get dropdownIconDisabledColor =>
      extractColor(prop('icon-disabled-color'));
  Color? get dropdownIconEnabledColor =>
      extractColor(prop('icon-enabled-color'));
  double get dropdownIconSize => extractDouble(prop('icon-size')) ?? 24.0;
  bool get dropdownIsDense => extractBool(prop('is-dense')) ?? false;
  bool get dropdownIsExpanded => extractBool(prop('is-expanded')) ?? false;
  double? get dropdownItemHeight => extractDouble(prop('item-height'));
  Color? get dropdownFocusColor => extractColor(prop('focus-color'));
  FocusNode? get dropdownFocusNode =>
      extractNativeValue<FocusNode>(prop('focus-node'));
  bool get dropdownAutofocus => extractBool(prop('autofocus')) ?? false;
  Color? get dropdownDropdownColor => extractColor(prop('dropdown-color'));
  double? get dropdownMenuMaxHeight => extractDouble(prop('menu-max-height'));
  bool get dropdownEnableFeedback =>
      extractBool(prop('enable-feedback')) ?? true;
  AlignmentGeometry get dropdownAlignment =>
      extractNativeValue<AlignmentGeometry>(prop('alignment')) ??
      Alignment.centerLeft;
  BorderRadius? get dropdownBorderRadius =>
      extractNativeValue<BorderRadius>(prop('border-radius'));
  EdgeInsetsGeometry? get dropdownPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding'));

  // RefreshIndicator properties
  double get refreshDisplacement => extractDouble(prop('displacement')) ?? 40.0;
  double get refreshEdgeOffset => extractDouble(prop('edge-offset')) ?? 0.0;
  RefreshCallback? get cupertinoRefreshOnRefresh =>
      extractNativeValue<RefreshCallback>(prop('on-refresh'));
  Color? get refreshColor => extractColor(prop('color'));
  Color? get refreshBackgroundColor => extractColor(prop('background-color'));
  ScrollNotificationPredicate get refreshNotificationPredicate =>
      extractNativeValue<ScrollNotificationPredicate>(
        prop('notification-predicate'),
      ) ??
      defaultScrollNotificationPredicate;
  String? get refreshSemanticsLabel => extractString(prop('semantics-label'));
  String? get refreshSemanticsValue => extractString(prop('semantics-value'));
  double get refreshStrokeWidth => extractDouble(prop('stroke-width')) ?? 2.0;

  // CircularProgressIndicator properties
  double? get circularProgressValue => extractDouble(prop('value'));
  Color? get circularProgressBackgroundColor =>
      extractColor(prop('background-color'));
  double get circularProgressStrokeWidth =>
      extractDouble(prop('stroke-width')) ?? 4.0;
  double get circularProgressStrokeAlign =>
      extractDouble(prop('stroke-align')) ?? 0.0;
  StrokeCap get circularProgressStrokeCap =>
      extractNativeValue<StrokeCap>(prop('stroke-cap')) ?? StrokeCap.round;
  String? get circularProgressSemanticsLabel =>
      extractString(prop('semantics-label'));
  String? get circularProgressSemanticsValue =>
      extractString(prop('semantics-value'));

  // Stepper properties
  int get stepperCurrentStep => extractInt(prop('current-step')) ?? 0;
  ValueChanged<int>? get stepperOnStepTapped =>
      extractNativeValue<ValueChanged<int>>(prop('on-step-tapped'));
  VoidCallback? get stepperOnStepContinue =>
      extractNativeValue<VoidCallback>(prop('on-step-continue'));
  VoidCallback? get stepperOnStepCancel =>
      extractNativeValue<VoidCallback>(prop('on-step-cancel'));
  ScrollPhysics? get stepperPhysics =>
      extractNativeValue<ScrollPhysics>(prop('physics'));
  double get stepperElevation => extractDouble(prop('elevation')) ?? 0.0;
  EdgeInsetsGeometry get stepperMargin =>
      extractNativeValue<EdgeInsetsGeometry>(prop('margin')) ?? EdgeInsets.zero;
  WidgetStateProperty<Color>? get stepperConnectorColor =>
      extractNativeValue<WidgetStateProperty<Color>>(prop('connector-color'));
  double get stepperConnectorThickness =>
      extractDouble(prop('connector-thickness')) ?? 1.0;
  double get stepperStepContent => extractDouble(prop('step-content')) ?? 0.0;

  // ExpansionPanelList properties
  Duration get expansionPanelListAnimationDuration =>
      extractNativeValue<Duration>(prop('animation-duration')) ??
      const Duration(milliseconds: 200);
  double get expansionPanelListElevation =>
      extractDouble(prop('elevation')) ?? 2.0;
  double get expansionPanelListMaterialGapSize =>
      extractDouble(prop('material-gap-size')) ?? 16.0;
  Color? get expansionPanelListDividerColor =>
      extractColor(prop('divider-color'));
  Color? get expansionPanelListExpandIconColor =>
      extractColor(prop('expand-icon-color'));

  // TabBarView properties
  List<Widget>? get tabBarViewChildren => extractChildren(prop('children'));
  ScrollPhysics? get tabBarViewPhysics =>
      extractNativeValue<ScrollPhysics>(prop('physics'));
  DragStartBehavior get tabBarViewDragStartBehavior =>
      extractNativeValue<DragStartBehavior>(prop('drag-start-behavior')) ??
      DragStartBehavior.start;
  double get tabBarViewViewportFraction =>
      extractDouble(prop('viewport-fraction')) ?? 1.0;

  // BottomSheet properties
  AnimationController? get bottomSheetAnimationController =>
      extractNativeValue<AnimationController>(prop('animation-controller'));
  bool get bottomSheetEnableDrag => extractBool(prop('enable-drag')) ?? true;
  bool? get bottomSheetShowDragHandle => extractBool(prop('show-drag-handle'));
  Color? get bottomSheetDragHandleColor =>
      extractColor(prop('drag-handle-color'));
  Size? get bottomSheetDragHandleSize =>
      extractNativeValue<Size>(prop('drag-handle-size'));
  VoidCallback get bottomSheetOnClosing =>
      extractNativeValue<VoidCallback>(prop('on-closing')) ?? () {};
  WidgetBuilder get bottomSheetBuilder =>
      extractNativeValue<WidgetBuilder>(prop('builder')) ??
      (_) => const SizedBox();

  // SearchBar properties
  FocusNode? get searchBarFocusNode =>
      extractNativeValue<FocusNode>(prop('focus-node'));
  String? get searchBarHintText => extractString(prop('hint-text'));
  Widget? get searchBarLeading => extractNativeValue<Widget>(prop('leading'));
  Iterable<Widget>? get searchBarTrailing => extractChildren(prop('trailing'));
  VoidCallback? get searchBarOnTap =>
      extractNativeValue<VoidCallback>(prop('on-press'));
  ValueChanged<String>? get searchBarOnChanged =>
      extractNativeValue<ValueChanged<String>>(prop('on-changed'));
  ValueChanged<String>? get searchBarOnSubmitted =>
      extractNativeValue<ValueChanged<String>>(prop('on-submitted'));
  BoxConstraints? get searchBarConstraints =>
      extractNativeValue<BoxConstraints>(prop('constraints'));
  WidgetStateProperty<double?>? get searchBarElevation =>
      extractNativeValue<WidgetStateProperty<double?>>(prop('elevation'));
  WidgetStateProperty<Color?>? get searchBarBackgroundColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('background-color'));
  WidgetStateProperty<Color?>? get searchBarShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('shadow-color'));
  WidgetStateProperty<Color?>? get searchBarSurfaceTintColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        prop('surface-tint-color'),
      );
  WidgetStateProperty<Color?>? get searchBarOverlayColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('overlay-color'));
  WidgetStateProperty<BorderSide?>? get searchBarSide =>
      extractNativeValue<WidgetStateProperty<BorderSide?>>(prop('side'));
  WidgetStateProperty<OutlinedBorder?>? get searchBarShape =>
      extractNativeValue<WidgetStateProperty<OutlinedBorder?>>(prop('shape'));
  EdgeInsetsGeometry? get searchBarPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding'));
  TextStyle? get searchBarTextStyle =>
      extractNativeValue<TextStyle>(prop('text-style'));
  TextStyle? get searchBarHintStyle =>
      extractNativeValue<TextStyle>(prop('hint-style'));
  TextCapitalization get searchBarTextCapitalization =>
      extractNativeValue<TextCapitalization>(prop('text-capitalization')) ??
      TextCapitalization.none;
  TextInputType? get searchBarKeyboardType =>
      extractNativeValue<TextInputType>(prop('keyboard-type'));
  Widget Function(Iterable<Widget> suggestions)? get searchBarViewBuilder =>
      extractNativeValue<Widget Function(Iterable<Widget> suggestions)>(
        prop('view-builder'),
      );
  BoxConstraints? get searchBarViewConstraints =>
      extractNativeValue<BoxConstraints>(prop('view-constraints'));
  double? get searchBarViewElevation => extractDouble(prop('view-elevation'));
  Color? get searchBarViewBackgroundColor =>
      extractColor(prop('view-background-color'));
  Color? get searchBarViewShadowColor =>
      extractColor(prop('view-shadow-color'));
  Color? get searchBarViewSurfaceTintColor =>
      extractColor(prop('view-surface-tint-color'));
  OutlinedBorder? get searchBarViewShape =>
      extractNativeValue<OutlinedBorder>(prop('view-shape'));
  BorderSide? get searchBarViewSide =>
      extractNativeValue<BorderSide>(prop('view-side'));
  EdgeInsetsGeometry? get searchBarViewPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('view-padding'));
  Widget? get searchBarViewLeading =>
      extractNativeValue<Widget>(prop('view-leading'));
  Iterable<Widget>? get searchBarViewTrailing =>
      extractChildren(prop('view-trailing'));
  String? get searchBarViewHintText => extractString(prop('view-hint-text'));
  TextStyle? get searchBarViewHintStyle =>
      extractNativeValue<TextStyle>(prop('view-hint-style'));
  TapRegionCallback? get searchBarOnTapOutside =>
      extractNativeValue<TapRegionCallback>(prop('on-tap-outside'));

  // SearchAnchor properties
  WidgetBuilder? get searchAnchorBuilder =>
      extractNativeValue<WidgetBuilder>(prop('builder'));
  BoxConstraints? get searchAnchorViewConstraints =>
      extractNativeValue<BoxConstraints>(prop('view-constraints'));
  double? get searchAnchorViewElevation =>
      extractDouble(prop('view-elevation'));
  Color? get searchAnchorViewBackgroundColor =>
      extractColor(prop('view-background-color'));
  Color? get searchAnchorViewShadowColor =>
      extractColor(prop('view-shadow-color'));
  Color? get searchAnchorViewSurfaceTintColor =>
      extractColor(prop('view-surface-tint-color'));
  OutlinedBorder? get searchAnchorViewShape =>
      extractNativeValue<OutlinedBorder>(prop('view-shape'));
  BorderSide? get searchAnchorViewSide =>
      extractNativeValue<BorderSide>(prop('view-side'));
  EdgeInsetsGeometry? get searchAnchorViewPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('view-padding'));
  Widget? get searchAnchorViewLeading =>
      extractNativeValue<Widget>(prop('view-leading'));
  Iterable<Widget>? get searchAnchorViewTrailing =>
      extractChildren(prop('view-trailing'));
  String? get searchAnchorViewHintText => extractString(prop('view-hint-text'));
  TextStyle? get searchAnchorViewHintStyle =>
      extractNativeValue<TextStyle>(prop('view-hint-style'));
  bool get searchAnchorIsFullScreen =>
      extractBool(prop('is-full-screen')) ?? false;
  Color? get searchAnchorDividerColor => extractColor(prop('divider-color'));

  // InputChip properties
  bool get inputChipSelected => extractBool(prop('selected')) ?? false;
  bool get inputChipIsEnabled => extractBool(prop('is-enabled')) ?? true;
  Widget? get inputChipLabel => extractNativeValue<Widget>(prop('label'));
  TextStyle? get inputChipLabelStyle =>
      extractNativeValue<TextStyle>(prop('label-style'));
  EdgeInsetsGeometry? get inputChipLabelPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('label-padding'));
  Widget? get inputChipDeleteIcon =>
      extractNativeValue<Widget>(prop('delete-icon'));
  VoidCallback? inputChipOnDeleted(Runtime runtime) =>
      extractVoidCallback(prop('on-deleted'), runtime);
  Color? get inputChipDeleteIconColor =>
      extractColor(prop('delete-icon-color'));
  String? get inputChipDeleteButtonTooltipMessage =>
      extractString(prop('delete-button-tooltip-message'));
  ValueChanged<bool>? get inputChipOnSelected =>
      extractNativeValue<ValueChanged<bool>>(prop('on-selected'));
  VoidCallback? inputChipOnPressed(Runtime runtime) =>
      extractVoidCallback(prop('on-pressed'), runtime);
  double? get inputChipPressElevation => extractDouble(prop('press-elevation'));
  Widget? get inputChipAvatar => extractNativeValue<Widget>(prop('avatar'));
  BoxConstraints? get inputChipAvatarBoxConstraints =>
      extractNativeValue<BoxConstraints>(prop('avatar-box-constraints'));
  ShapeBorder? get inputChipAvatarBorderProperty =>
      extractNativeValue<ShapeBorder>(prop('avatar-border'));
  BorderSide? get inputChipSide => extractNativeValue<BorderSide>(prop('side'));
  OutlinedBorder? get inputChipShape =>
      extractNativeValue<OutlinedBorder>(prop('shape'));
  Clip get inputChipClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.none;
  FocusNode? get inputChipFocusNode =>
      extractNativeValue<FocusNode>(prop('focus-node'));
  bool get inputChipAutofocus => extractBool(prop('autofocus')) ?? false;
  Color? get inputChipBackgroundColor => extractColor(prop('background-color'));
  Color? get inputChipDisabledColor => extractColor(prop('disabled-color'));
  Color? get inputChipSelectedColor => extractColor(prop('selected-color'));
  Color? get inputChipCheckmarkColor => extractColor(prop('checkmark-color'));
  bool? get inputChipShowCheckmark => extractBool(prop('show-checkmark'));
  WidgetStateProperty<Color?>? get inputChipColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('color'));
  WidgetStateProperty<Color?>? get inputChipSurfaceTintColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        prop('surface-tint-color'),
      );
  WidgetStateProperty<double?>? get inputChipElevation =>
      extractNativeValue<WidgetStateProperty<double?>>(prop('elevation'));
  WidgetStateProperty<Color?>? get inputChipShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('shadow-color'));
  WidgetStateProperty<Color?>? get inputChipSelectedShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        prop('selected-shadow-color'),
      );
  // FilterChip properties
  bool get filterChipSelected => extractBool(prop('selected')) ?? false;
  Widget? get filterChipLabel => extractNativeValue<Widget>(prop('label'));
  TextStyle? get filterChipLabelStyle =>
      extractNativeValue<TextStyle>(prop('label-style'));
  EdgeInsetsGeometry? get filterChipLabelPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('label-padding'));
  Widget? get filterChipAvatar => extractNativeValue<Widget>(prop('avatar'));
  BoxConstraints? get filterChipAvatarBoxConstraints =>
      extractNativeValue<BoxConstraints>(prop('avatar-box-constraints'));
  ShapeBorder? get filterChipAvatarBorder =>
      extractNativeValue<ShapeBorder>(prop('avatar-border'));
  Widget? get filterChipDeleteIcon =>
      extractNativeValue<Widget>(prop('delete-icon'));
  VoidCallback? get filterChipOnDeleted =>
      extractNativeValue<VoidCallback>(prop('on-deleted'));
  Color? get filterChipDeleteIconColor =>
      extractColor(prop('delete-icon-color'));
  String? get filterChipDeleteButtonTooltipMessage =>
      extractString(prop('delete-button-tooltip-message'));
  ValueChanged<bool>? get filterChipOnSelected =>
      extractNativeValue<ValueChanged<bool>>(prop('on-selected'));
  VoidCallback? get filterChipOnPressed =>
      extractNativeValue<VoidCallback>(prop('on-pressed'));
  double? get filterChipPressElevation =>
      extractDouble(prop('press-elevation'));
  BorderSide? get filterChipSide =>
      extractNativeValue<BorderSide>(prop('side'));
  OutlinedBorder? get filterChipShape =>
      extractNativeValue<OutlinedBorder>(prop('shape'));
  Clip get filterChipClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.none;
  FocusNode? get filterChipFocusNode =>
      extractNativeValue<FocusNode>(prop('focus-node'));
  bool get filterChipAutofocus => extractBool(prop('autofocus')) ?? false;
  Color? get filterChipBackgroundColor =>
      extractColor(prop('background-color'));
  Color? get filterChipDisabledColor => extractColor(prop('disabled-color'));
  Color? get filterChipSelectedColor => extractColor(prop('selected-color'));
  Color? get filterChipCheckmarkColor => extractColor(prop('checkmark-color'));
  bool? get filterChipShowCheckmark => extractBool(prop('show-checkmark'));
  WidgetStateProperty<Color?>? get filterChipColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('color'));
  WidgetStateProperty<Color?>? get filterChipSurfaceTintColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        prop('surface-tint-color'),
      );
  WidgetStateProperty<double?>? get filterChipElevation =>
      extractNativeValue<WidgetStateProperty<double?>>(prop('elevation'));
  WidgetStateProperty<Color?>? get filterChipShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('shadow-color'));
  WidgetStateProperty<Color?>? get filterChipSelectedShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        prop('selected-shadow-color'),
      );
  // ChoiceChip properties
  bool get choiceChipSelected => extractBool(prop('selected')) ?? false;
  Widget? get choiceChipLabel => extractNativeValue<Widget>(prop('label'));
  TextStyle? get choiceChipLabelStyle =>
      extractNativeValue<TextStyle>(prop('label-style'));
  EdgeInsetsGeometry? get choiceChipLabelPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('label-padding'));
  Widget? get choiceChipAvatar => extractNativeValue<Widget>(prop('avatar'));
  BoxConstraints? get choiceChipAvatarBoxConstraints =>
      extractNativeValue<BoxConstraints>(prop('avatar-box-constraints'));
  ShapeBorder? get choiceChipAvatarBorder =>
      extractNativeValue<ShapeBorder>(prop('avatar-border'));
  ValueChanged<bool>? get choiceChipOnSelected =>
      extractNativeValue<ValueChanged<bool>>(prop('on-selected'));
  double? get choiceChipPressElevation =>
      extractDouble(prop('press-elevation'));
  BorderSide? get choiceChipSide =>
      extractNativeValue<BorderSide>(prop('side'));
  OutlinedBorder? get choiceChipShape =>
      extractNativeValue<OutlinedBorder>(prop('shape'));
  Clip get choiceChipClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.none;
  FocusNode? get choiceChipFocusNode =>
      extractNativeValue<FocusNode>(prop('focus-node'));
  bool get choiceChipAutofocus => extractBool(prop('autofocus')) ?? false;
  Color? get choiceChipBackgroundColor =>
      extractColor(prop('background-color'));
  Color? get choiceChipDisabledColor => extractColor(prop('disabled-color'));
  Color? get choiceChipSelectedColor => extractColor(prop('selected-color'));
  Color? get choiceChipCheckmarkColor => extractColor(prop('checkmark-color'));
  bool? get choiceChipShowCheckmark => extractBool(prop('show-checkmark'));
  WidgetStateProperty<Color?>? get choiceChipColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('color'));
  WidgetStateProperty<Color?>? get choiceChipSurfaceTintColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        prop('surface-tint-color'),
      );
  WidgetStateProperty<double?>? get choiceChipElevation =>
      extractNativeValue<WidgetStateProperty<double?>>(prop('elevation'));
  WidgetStateProperty<Color?>? get choiceChipShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('shadow-color'));
  WidgetStateProperty<Color?>? get choiceChipSelectedShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        prop('selected-shadow-color'),
      );
  // ActionChip properties
  Widget? get actionChipLabel => extractNativeValue<Widget>(prop('label'));
  TextStyle? get actionChipLabelStyle =>
      extractNativeValue<TextStyle>(prop('label-style'));
  EdgeInsetsGeometry? get actionChipLabelPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('label-padding'));
  Widget? get actionChipAvatar => extractNativeValue<Widget>(prop('avatar'));
  BoxConstraints? get actionChipAvatarBoxConstraints =>
      extractNativeValue<BoxConstraints>(prop('avatar-box-constraints'));
  ShapeBorder? get actionChipAvatarBorder =>
      extractNativeValue<ShapeBorder>(prop('avatar-border'));
  VoidCallback? get actionChipOnPressed =>
      extractNativeValue<VoidCallback>(prop('on-pressed'));
  double? get actionChipPressElevation =>
      extractDouble(prop('press-elevation'));
  BorderSide? get actionChipSide =>
      extractNativeValue<BorderSide>(prop('side'));
  OutlinedBorder? get actionChipShape =>
      extractNativeValue<OutlinedBorder>(prop('shape'));
  Clip get actionChipClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.none;
  FocusNode? get actionChipFocusNode =>
      extractNativeValue<FocusNode>(prop('focus-node'));
  bool get actionChipAutofocus => extractBool(prop('autofocus')) ?? false;
  Color? get actionChipBackgroundColor =>
      extractColor(prop('background-color'));
  Color? get actionChipDisabledColor => extractColor(prop('disabled-color'));
  WidgetStateProperty<double?>? get actionChipElevation =>
      extractNativeValue<WidgetStateProperty<double?>>(prop('elevation'));
  WidgetStateProperty<Color?>? get actionChipShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(prop('shadow-color'));
  WidgetStateProperty<Color?>? get actionChipSurfaceTintColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        prop('surface-tint-color'),
      );

  // BottomAppBar properties
  Color? get bottomAppBarColor => extractColor(prop('color'));
  double get bottomAppBarElevation => extractDouble(prop('elevation')) ?? 8.0;
  ShapeBorder? get bottomAppBarShape =>
      extractNativeValue<ShapeBorder>(prop('shape'));
  Clip get bottomAppBarClipBehavior =>
      extractNativeValue<Clip>(prop('clip-behavior')) ?? Clip.none;
  double get bottomAppBarNotchMargin =>
      extractDouble(prop('notch-margin')) ?? 4.0;
  double? get bottomAppBarHeight => extractDouble(prop('height'));
  EdgeInsetsGeometry? get bottomAppBarPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding'));
  Color? get bottomAppBarSurfaceTintColor =>
      extractColor(prop('surface-tint-color'));
  Color? get bottomAppBarShadowColor => extractColor(prop('shadow-color'));
  Widget? get bottomAppBarChild => extractNativeValue<Widget>(prop('child'));

  // NavigationDrawer properties
  Color? get navigationDrawerBackgroundColor =>
      extractColor(prop('background-color'));
  double get navigationDrawerElevation =>
      extractDouble(prop('elevation')) ?? 1.0;
  Color? get navigationDrawerShadowColor => extractColor(prop('shadow-color'));
  Color? get navigationDrawerSurfaceTintColor =>
      extractColor(prop('surface-tint-color'));
  Color? get navigationDrawerIndicatorColor =>
      extractColor(prop('indicator-color'));
  ShapeBorder? get navigationDrawerIndicatorShape =>
      extractNativeValue<ShapeBorder>(prop('indicator-shape'));
  int get navigationDrawerSelectedIndex =>
      extractInt(prop('selected-index')) ?? 0;
  ValueChanged<int>? get navigationDrawerOnDestinationSelected =>
      extractNativeValue<ValueChanged<int>>(prop('on-destination-selected'));
  List<Widget>? get navigationDrawerChildren =>
      extractChildren(prop('children'));
  EdgeInsetsGeometry? get navigationDrawerTilePadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('tile-padding'));

  // DrawerHeader properties
  Decoration? get drawerHeaderDecoration =>
      extractNativeValue<Decoration>(prop('decoration'));
  EdgeInsetsGeometry? get drawerHeaderMargin =>
      extractNativeValue<EdgeInsetsGeometry>(prop('margin'));
  EdgeInsetsGeometry? get drawerHeaderPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding'));
  Duration? get drawerHeaderDuration =>
      extractNativeValue<Duration>(prop('duration'));
  Curve? get drawerHeaderCurve => extractNativeValue<Curve>(prop('curve'));
  Widget? get drawerHeaderChild => extractNativeValue<Widget>(prop('child'));

  // UserAccountsDrawerHeader properties
  Decoration? get userAccountsDrawerHeaderDecoration =>
      extractNativeValue<Decoration>(prop('decoration'));
  EdgeInsetsGeometry? get userAccountsDrawerHeaderMargin =>
      extractNativeValue<EdgeInsetsGeometry>(prop('margin'));
  EdgeInsetsGeometry? get userAccountsDrawerHeaderPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding'));
  Widget? get userAccountsDrawerHeaderCurrentAccountPicture =>
      extractNativeValue<Widget>(prop('current-account-picture'));
  List<Widget>? get userAccountsDrawerHeaderOtherAccountsPictures =>
      extractChildren(prop('other-accounts-pictures'));
  Widget? get userAccountsDrawerHeaderAccountName =>
      extractNativeValue<Widget>(prop('account-name'));
  Widget? get userAccountsDrawerHeaderAccountEmail =>
      extractNativeValue<Widget>(prop('account-email'));
  VoidCallback? get userAccountsDrawerHeaderOnDetailsPressed =>
      extractNativeValue<VoidCallback>(prop('on-details-pressed'));
  Color? get userAccountsDrawerHeaderArrowColor =>
      extractColor(prop('arrow-color'));

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

  // NavigationRail properties
  Color? get navigationRailBackgroundColor =>
      extractColor(prop('background-color'));
  bool get navigationRailExtended => extractBool(prop('extended')) ?? false;
  Widget? get navigationRailLeading =>
      extractNativeValue<Widget>(prop('leading'));
  Widget? get navigationRailTrailing =>
      extractNativeValue<Widget>(prop('trailing'));
  int? get navigationRailSelectedIndex => extractInt(prop('selected-index'));
  ValueChanged<int>? get navigationRailOnDestinationSelected =>
      extractNativeValue<ValueChanged<int>>(prop('on-destination-selected'));
  double get navigationRailElevation => extractDouble(prop('elevation')) ?? 0.0;
  double get navigationRailGroupAlignment =>
      extractDouble(prop('group-alignment')) ?? -1.0;
  TextStyle? get navigationRailUnselectedLabelTextStyle =>
      extractNativeValue<TextStyle>(prop('unselected-label-text-style'));
  TextStyle? get navigationRailSelectedLabelTextStyle =>
      extractNativeValue<TextStyle>(prop('selected-label-text-style'));
  IconThemeData? get navigationRailUnselectedIconTheme =>
      extractNativeValue<IconThemeData>(prop('unselected-icon-theme'));
  IconThemeData? get navigationRailSelectedIconTheme =>
      extractNativeValue<IconThemeData>(prop('selected-icon-theme'));
  double get navigationRailMinWidth => extractDouble(prop('min-width')) ?? 72.0;
  double get navigationRailMinExtendedWidth =>
      extractDouble(prop('min-extended-width')) ?? 256.0;
  bool get navigationRailUseIndicator =>
      extractBool(prop('use-indicator')) ?? true;
  Color? get navigationRailIndicatorColor =>
      extractColor(prop('indicator-color'));
  ShapeBorder? get navigationRailIndicatorShape =>
      extractNativeValue<ShapeBorder>(prop('indicator-shape'));
  bool get navigationRailLeadingAtTop =>
      extractBool(prop('leading-at-top')) ?? true;
  bool get navigationRailTrailingAtBottom =>
      extractBool(prop('trailing-at-bottom')) ?? false;
  bool get navigationRailScrollable => extractBool(prop('scrollable')) ?? false;

  // CupertinoButton properties
  CupertinoButtonSize get cupertinoButtonSizeStyle =>
      extractNativeValue<CupertinoButtonSize>(prop('size-style')) ??
      CupertinoButtonSize.large;
  EdgeInsetsGeometry get cupertinoButtonPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding')) ??
      const EdgeInsets.symmetric(vertical: 16.0, horizontal: 30.0);
  Color get cupertinoButtonDisabledColor =>
      extractColor(prop('disabled-color')) ??
      CupertinoColors.quaternarySystemFill;
  double get cupertinoButtonPressedOpacity =>
      extractDouble(prop('pressed-opacity')) ?? 0.4;
  BorderRadius get cupertinoButtonBorderRadius =>
      extractNativeValue<BorderRadius>(prop('border-radius')) ??
      const BorderRadius.all(Radius.circular(8.0));

  // CupertinoActivityIndicator properties
  bool get cupertinoActivityIndicatorAnimating =>
      extractBool(prop('animating')) ?? true;
  double get cupertinoActivityIndicatorRadius =>
      extractDouble(prop('radius')) ?? 10.0;

  // CupertinoNavigationBar properties
  Widget? get cupertinoNavigationBarLeading =>
      extractNativeValue<Widget>(prop('leading'));
  bool get cupertinoNavigationBarAutomaticallyImplyLeading =>
      extractBool(prop('automatically-imply-leading')) ?? true;
  bool get cupertinoNavigationBarAutomaticallyImplyMiddle =>
      extractBool(prop('automatically-imply-middle')) ?? true;
  String? get cupertinoNavigationBarPreviousPageTitle =>
      extractString(prop('previous-page-title'));
  Widget get cupertinoNavigationBarMiddle =>
      extractNativeValue<Widget>(prop('middle'))!;
  Widget? get cupertinoNavigationBarTrailing =>
      extractNativeValue<Widget>(prop('trailing'));
  Border get cupertinoNavigationBarBorder =>
      extractNativeValue<Border>(prop('border')) ??
      const Border(bottom: BorderSide(color: Color(0x4D000000), width: 0.0));
  Color? get cupertinoNavigationBarBackgroundColor =>
      extractColor(prop('background-color'));
  Brightness? get cupertinoNavigationBarBrightness =>
      extractNativeValue<Brightness>(prop('brightness'));
  EdgeInsetsDirectional get cupertinoNavigationBarPadding =>
      extractNativeValue<EdgeInsetsDirectional>(prop('padding')) ??
      EdgeInsetsDirectional.zero;
  bool get cupertinoNavigationBarTransitionBetweenRoutes =>
      extractBool(prop('transition-between-routes')) ?? true;
  Object get cupertinoNavigationBarHeroTag =>
      extractNativeValue<Object>(prop('hero-tag')) ?? _defaultHeroTag;

  // CupertinoPageScaffold properties
  Color? get cupertinoPageScaffoldBackgroundColor =>
      extractColor(prop('background-color'));
  bool get cupertinoPageScaffoldResizeToAvoidBottomInset =>
      extractBool(prop('resize-to-avoid-bottom-inset')) ?? true;

  // CupertinoTextField properties
  TextEditingController? get cupertinoTextFieldController =>
      extractNativeValue<TextEditingController>(prop('controller'));
  String? get cupertinoTextFieldPlaceholder =>
      extractString(prop('placeholder'));
  TextStyle get cupertinoTextFieldPlaceholderStyle =>
      extractNativeValue<TextStyle>(prop('placeholder-style')) ??
      const TextStyle(
        fontWeight: FontWeight.w400,
        color: CupertinoColors.placeholderText,
      );
  Widget? get cupertinoTextFieldPrefix =>
      extractNativeValue<Widget>(prop('prefix'));
  OverlayVisibilityMode get cupertinoTextFieldPrefixMode =>
      extractNativeValue<OverlayVisibilityMode>(prop('prefix-mode')) ??
      OverlayVisibilityMode.always;
  Widget? get cupertinoTextFieldSuffix =>
      extractNativeValue<Widget>(prop('suffix'));
  OverlayVisibilityMode get cupertinoTextFieldSuffixMode =>
      extractNativeValue<OverlayVisibilityMode>(prop('suffix-mode')) ??
      OverlayVisibilityMode.always;
  OverlayVisibilityMode get cupertinoTextFieldClearButtonMode =>
      extractNativeValue<OverlayVisibilityMode>(prop('clear-button-mode')) ??
      OverlayVisibilityMode.never;
  Color get cupertinoTextFieldDecorationBorderColor =>
      extractColor(prop('decoration-border-color')) ??
      CupertinoColors.inactiveGray;
  BoxDecoration? get cupertinoTextFieldDecoration =>
      extractNativeValue<BoxDecoration>(prop('decoration')) ??
      const BoxDecoration(
        border: Border.fromBorderSide(
          BorderSide(width: 0.0, color: CupertinoColors.inactiveGray),
        ),
        borderRadius: BorderRadius.all(Radius.circular(5.0)),
      );
  EdgeInsetsGeometry get cupertinoTextFieldPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding')) ??
      const EdgeInsets.all(6.0);

  // CupertinoSwitch properties
  bool get cupertinoSwitchValue => extractBool(prop('value')) ?? false;

  // CupertinoDatePicker properties
  CupertinoDatePickerMode get cupertinoDatePickerMode =>
      extractNativeValue<CupertinoDatePickerMode>(prop('mode')) ??
      CupertinoDatePickerMode.dateAndTime;
  ValueChanged<DateTime> get cupertinoDatePickerOnDateTimeChanged =>
      extractNativeValue<ValueChanged<DateTime>>(prop('on-date-time-changed'))!;
  DateTime? get cupertinoDatePickerInitialDateTime =>
      extractNativeValue<DateTime>(prop('initial-date-time'));
  DateTime? get cupertinoDatePickerMinimumDate =>
      extractNativeValue<DateTime>(prop('minimum-date'));
  DateTime? get cupertinoDatePickerMaximumDate =>
      extractNativeValue<DateTime>(prop('maximum-date'));
  int get cupertinoDatePickerMinimumYear =>
      extractInt(prop('minimum-year')) ?? 1;
  int? get cupertinoDatePickerMaximumYear => extractInt(prop('maximum-year'));
  int get cupertinoDatePickerMinuteInterval =>
      extractInt(prop('minute-interval')) ?? 1;
  bool get cupertinoDatePickerUse24hFormat =>
      extractBool(prop('use-24h-format')) ?? false;
  DatePickerDateOrder? get cupertinoDatePickerDateOrder =>
      extractNativeValue<DatePickerDateOrder>(prop('date-order'));
  Color? get cupertinoDatePickerBackgroundColor =>
      extractColor(prop('background-color'));
  bool get cupertinoDatePickerShowDayOfWeek =>
      extractBool(prop('show-day-of-week')) ?? false;
  double get cupertinoDatePickerItemExtent =>
      extractDouble(prop('item-extent')) ?? 32.0;

  // CupertinoPicker properties
  double get cupertinoPickerDiameterRatio =>
      extractDouble(prop('diameter-ratio')) ?? 1.07;
  Color? get cupertinoPickerBackgroundColor =>
      extractColor(prop('background-color'));
  double get cupertinoPickerOffAxisFraction =>
      extractDouble(prop('off-axis-fraction')) ?? 0.0;
  bool get cupertinoPickerUseMagnifier =>
      extractBool(prop('use-magnifier')) ?? false;
  double get cupertinoPickerMagnification =>
      extractDouble(prop('magnification')) ?? 1.0;
  FixedExtentScrollController? get cupertinoPickerScrollController =>
      extractNativeValue<FixedExtentScrollController>(
        prop('scroll-controller'),
      );
  double get cupertinoPickerSqueeze => extractDouble(prop('squeeze')) ?? 1.45;
  double get cupertinoPickerItemExtent =>
      extractDouble(prop('item-extent')) ?? 32.0;
  ValueChanged<int>? get cupertinoPickerOnSelectedItemChanged =>
      extractNativeValue<ValueChanged<int>>(prop('on-selected-item-changed'));
  CupertinoPickerDefaultSelectionOverlay? get cupertinoPickerSelectionOverlay =>
      extractNativeValue<CupertinoPickerDefaultSelectionOverlay>(
        prop('selection-overlay'),
      );

  // CupertinoScrollable properties
  ScrollController? get cupertinoScrollableController =>
      extractNativeValue<ScrollController>(prop('controller'));
  double get cupertinoScrollableThickness =>
      extractDouble(prop('thickness')) ?? 3.0;
  double get cupertinoScrollableThicknessWhileDragging =>
      extractDouble(prop('thickness-while-dragging')) ?? 8.0;
  Radius get cupertinoScrollableRadius =>
      extractNativeValue<Radius>(prop('radius')) ?? const Radius.circular(1.5);
  Radius get cupertinoScrollableRadiusWhileDragging =>
      extractNativeValue<Radius>(prop('radius-while-dragging')) ??
      const Radius.circular(4.0);
  ScrollNotificationPredicate? get cupertinoScrollableNotificationPredicate =>
      extractNativeValue<ScrollNotificationPredicate>(
        prop('notification-predicate'),
      );

  // CupertinoSearchTextField properties
  SearchController? get cupertinoSearchTextFieldController =>
      extractNativeValue<SearchController>(prop('controller'));
  ValueChanged<String>? get cupertinoSearchTextFieldOnChanged =>
      extractNativeValue<ValueChanged<String>>(prop('on-changed'));
  ValueChanged<String>? get cupertinoSearchTextFieldOnSubmitted =>
      extractNativeValue<ValueChanged<String>>(prop('on-submitted'));
  TextStyle? get cupertinoSearchTextFieldStyle =>
      extractNativeValue<TextStyle>(prop('style'));
  String? get cupertinoSearchTextFieldHintText =>
      extractString(prop('hint-text'));
  Widget? get cupertinoSearchTextFieldPrefixIcon =>
      extractNativeValue<Widget>(prop('prefix-icon'));
  Widget? get cupertinoSearchTextFieldSuffixIcon =>
      extractNativeValue<Widget>(prop('suffix-icon'));
  OverlayVisibilityMode get cupertinoSearchTextFieldPrefixMode =>
      extractNativeValue<OverlayVisibilityMode>(prop('prefix-mode')) ??
      OverlayVisibilityMode.always;
  OverlayVisibilityMode get cupertinoSearchTextFieldSuffixMode =>
      extractNativeValue<OverlayVisibilityMode>(prop('suffix-mode')) ??
      OverlayVisibilityMode.always;
  VoidCallback? get cupertinoSearchTextFieldOnSuffixTap =>
      extractNativeValue<VoidCallback>(prop('on-suffix-tap'));
  BoxDecoration? get cupertinoSearchTextFieldDecoration =>
      extractNativeValue<BoxDecoration>(prop('decoration'));
  Color? get cupertinoSearchTextFieldBackgroundColor =>
      extractColor(prop('background-color'));
  BorderRadius? get cupertinoSearchTextFieldBorderRadius =>
      extractNativeValue<BorderRadius>(prop('border-radius'));
  EdgeInsetsGeometry get cupertinoSearchTextFieldPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding')) ??
      const EdgeInsetsDirectional.fromSTEB(5.5, 8, 5.5, 8);
  Color? get cupertinoSearchTextFieldItemColor =>
      extractColor(prop('item-color'));
  double? get cupertinoSearchTextFieldItemSize =>
      extractDouble(prop('item-size'));

  // CupertinoSegmentedControl properties
  Map<Object, Widget> get cupertinoSegmentedControlChildren =>
      extractNativeValue<Map<Object, Widget>>(prop('children')) ?? {};
  ValueChanged<Object>? get cupertinoSegmentedControlOnValueChanged =>
      extractNativeValue<ValueChanged<Object>>(prop('on-value-changed'));
  Object? get cupertinoSegmentedControlGroupValue =>
      extractNativeValue<Object>(prop('group-value'));
  Color get cupertinoSegmentedControlUnselectedColor =>
      extractColor(prop('unselected-color')) ??
      CupertinoColors.tertiarySystemFill;
  Color get cupertinoSegmentedControlSelectedColor =>
      extractColor(prop('selected-color')) ?? CupertinoColors.systemBlue;
  Color get cupertinoSegmentedControlBorderColor =>
      extractColor(prop('border-color')) ?? CupertinoColors.systemGrey4;
  Color? get cupertinoSegmentedControlPressedColor =>
      extractColor(prop('pressed-color'));
  EdgeInsetsGeometry get cupertinoSegmentedControlPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding')) ??
      const EdgeInsets.symmetric(vertical: 2, horizontal: 3);

  // CupertinoSlider properties
  double get cupertinoSliderValue => extractDouble(prop('value')) ?? 0.0;
  double get cupertinoSliderMin => extractDouble(prop('min')) ?? 0.0;
  double get cupertinoSliderMax => extractDouble(prop('max')) ?? 1.0;
  int? get cupertinoSliderDivisions => extractInt(prop('divisions'));
  Color? get cupertinoSliderActiveColor => extractColor(prop('active-color'));
  Color get cupertinoSliderThumbColor =>
      extractColor(prop('thumb-color')) ?? CupertinoColors.white;
  // CupertinoSlidingSegmentedControl properties
  Map<dynamic, Widget> get cupertinoSlidingSegmentedControlChildren =>
      extractNativeValue<Map<dynamic, Widget>>(prop('children')) ?? {};
  ValueChanged<dynamic>? get cupertinoSlidingSegmentedControlOnValueChanged =>
      extractNativeValue<ValueChanged<dynamic>>(prop('on-value-changed'));
  dynamic get cupertinoSlidingSegmentedControlGroupValue =>
      extractNativeValue<dynamic>(prop('group-value'));
  Color get cupertinoSlidingSegmentedControlThumbColor =>
      extractColor(prop('thumb-color')) ?? const Color(0xFFFFFFFF);
  Color? get cupertinoSlidingSegmentedControlBackgroundColor =>
      extractColor(prop('background-color'));
  EdgeInsetsGeometry get cupertinoSlidingSegmentedControlPadding =>
      extractNativeValue<EdgeInsetsGeometry>(prop('padding')) ??
      const EdgeInsets.symmetric(vertical: 2, horizontal: 3);

  // CupertinoTimerPicker properties
  CupertinoTimerPickerMode get cupertinoTimerPickerMode =>
      extractNativeValue<CupertinoTimerPickerMode>(prop('mode')) ??
      CupertinoTimerPickerMode.hms;
  Duration get cupertinoTimerPickerInitialTimerDuration =>
      extractNativeValue<Duration>(prop('initial-timer-duration')) ??
      Duration.zero;
  int get cupertinoTimerPickerMinuteInterval =>
      extractInt(prop('minute-interval')) ?? 1;
  int get cupertinoTimerPickerSecondInterval =>
      extractInt(prop('second-interval')) ?? 1;
  AlignmentGeometry get cupertinoTimerPickerAlignment =>
      extractNativeValue<AlignmentGeometry>(prop('alignment')) ??
      Alignment.center;
  Color? get cupertinoTimerPickerBackgroundColor =>
      extractColor(prop('background-color'));
  double get cupertinoTimerPickerItemExtent =>
      extractDouble(prop('item-extent')) ?? 32.0;
  ValueChanged<Duration> get cupertinoTimerPickerOnTimerDurationChanged =>
      extractNativeValue<ValueChanged<Duration>>(
        prop('on-timer-duration-changed'),
      )!;

  // CupertinoTabBar properties
  List<BottomNavigationBarItem> get cupertinoTabBarItems =>
      extractNativeValue<List<BottomNavigationBarItem>>(prop('items')) ?? [];
  ValueChanged<int>? get cupertinoTabBarOnTap =>
      extractNativeValue<ValueChanged<int>>(prop('on-press'));
  int get cupertinoTabBarCurrentIndex => extractInt(prop('current-index')) ?? 0;
  Color? get cupertinoTabBarBackgroundColor =>
      extractColor(prop('background-color'));
  Color get cupertinoTabBarActiveColor =>
      extractColor(prop('active-color')) ?? CupertinoColors.activeBlue;
  Color get cupertinoTabBarInactiveColor =>
      extractColor(prop('inactive-color')) ?? CupertinoColors.inactiveGray;
  double get cupertinoTabBarIconSize =>
      extractDouble(prop('icon-size')) ?? 30.0;
  Border get cupertinoTabBarBorder =>
      extractNativeValue<Border>(prop('border')) ??
      const Border(top: BorderSide(color: Color(0x4D000000), width: 0.0));

  // CupertinoTabScaffold properties
  CupertinoTabBar get cupertinoTabScaffoldTabBar =>
      extractNativeValue<CupertinoTabBar>(prop('tab-bar'))!;
  IndexedWidgetBuilder get cupertinoTabScaffoldTabBuilder =>
      extractNativeValue<IndexedWidgetBuilder>(prop('tab-builder'))!;
  CupertinoTabController? get cupertinoTabScaffoldController =>
      extractNativeValue<CupertinoTabController>(prop('controller'));
  Color? get cupertinoTabScaffoldBackgroundColor =>
      extractColor(prop('background-color'));
  bool get cupertinoTabScaffoldResizeToAvoidBottomInset =>
      extractBool(prop('resize-to-avoid-bottom-inset')) ?? true;
  String? get cupertinoTabScaffoldRestorationId =>
      extractString(prop('restoration-id'));

  // CupertinoContextMenu properties
  List<Widget> get cupertinoContextMenuActions =>
      extractChildren(prop('actions')) ?? [];
  Widget? get cupertinoContextMenuPreviewBuilder =>
      extractNativeValue<Widget>(prop('preview-builder'));

  // CupertinoActionSheet properties
  Widget? get cupertinoActionSheetTitle =>
      extractNativeValue<Widget>(prop('cupertino-action-sheet-title'));
  Widget? get cupertinoActionSheetMessage =>
      extractNativeValue<Widget>(prop('cupertino-action-sheet-message'));
  List<Widget>? get cupertinoActionSheetActions =>
      extractChildren(prop('cupertino-action-sheet-actions'));
  ScrollController? get cupertinoActionSheetMessageScrollController =>
      extractNativeValue<ScrollController>(
        prop('cupertino-action-sheet-message-scroll-controller'),
      );
  ScrollController? get cupertinoActionSheetActionScrollController =>
      extractNativeValue<ScrollController>(
        prop('cupertino-action-sheet-action-scroll-controller'),
      );
  Widget? get cupertinoActionSheetCancelButton =>
      extractNativeValue<Widget>(prop('cupertino-action-sheet-cancel-button'));

  // CupertinoAlertDialog properties
  Widget? get cupertinoAlertDialogTitle =>
      extractNativeValue<Widget>(prop('cupertino-alert-dialog-title'));
  Widget? get cupertinoAlertDialogContent =>
      extractNativeValue<Widget>(prop('cupertino-alert-dialog-content'));
  List<Widget> get cupertinoAlertDialogActions =>
      extractChildren(prop('cupertino-alert-dialog-actions')) ?? [];
  ScrollController? get cupertinoAlertDialogScrollController =>
      extractNativeValue<ScrollController>(
        prop('cupertino-alert-dialog-scroll-controller'),
      );
  ScrollController? get cupertinoAlertDialogActionScrollController =>
      extractNativeValue<ScrollController>(
        prop('cupertino-alert-dialog-action-scroll-controller'),
      );
  Duration get cupertinoAlertDialogInsetAnimationDuration =>
      extractNativeValue<Duration>(
        prop('cupertino-alert-dialog-inset-animation-duration'),
      ) ??
      const Duration(milliseconds: 100);
  Curve get cupertinoAlertDialogInsetAnimationCurve =>
      extractNativeValue<Curve>(
        prop('cupertino-alert-dialog-inset-animation-curve'),
      ) ??
      Curves.decelerate;

  // Generic scroll properties
  ScrollController? get scrollController =>
      extractNativeValue<ScrollController>(prop('scroll-controller'));
  ScrollNotificationPredicate get scrollNotificationPredicate =>
      extractNativeValue<ScrollNotificationPredicate>(
        prop('notification-predicate'),
      ) ??
      defaultScrollNotificationPredicate;

  // Generic alert dialog properties
  Widget? get alertDialogTitle => extractNativeValue<Widget>(prop('title'));
  Widget? get alertDialogContent => extractNativeValue<Widget>(prop('content'));

  // CupertinoScrollbar properties
  bool get cupertinoScrollbarThumbVisibility =>
      extractBool(prop('thumb-visibility')) ?? true;
  double get cupertinoScrollbarThickness =>
      extractDouble(prop('thickness')) ?? 3.0;
  double get cupertinoScrollbarThicknessWhileDragging =>
      extractDouble(prop('thickness-while-dragging')) ?? 8.0;
  Radius get cupertinoScrollbarRadius =>
      extractNativeValue<Radius>(prop('radius')) ?? const Radius.circular(1.5);
  Radius get cupertinoScrollbarRadiusWhileDragging =>
      extractNativeValue<Radius>(prop('radius-while-dragging')) ??
      const Radius.circular(4.0);

  // CupertinoSlider properties
  ValueChanged<double>? get cupertinoSliderOnChanged =>
      extractNativeValue<ValueChanged<double>>(prop('on-changed'));
  ValueChanged<double>? get cupertinoSliderOnChangeStart =>
      extractNativeValue<ValueChanged<double>>(prop('on-change-start'));
  ValueChanged<double>? get cupertinoSliderOnChangeEnd =>
      extractNativeValue<ValueChanged<double>>(prop('on-change-end'));

  // CupertinoPageScaffold properties
  Widget? get cupertinoPageScaffoldChild =>
      extractNativeValue<Widget>(prop('child'));
  ObstructingPreferredSizeWidget? get cupertinoPageScaffoldNavigationBar =>
      extractNativeValue<ObstructingPreferredSizeWidget>(
        prop('navigation-bar'),
      );
  // bool? get cupertinoPageScaffoldResizeToAvoidBottomInset =>
  //     extractBool(prop('resize-to-avoid-bottom-inset'));
  // Widget? get cupertinoPageScaffoldChild => extractNativeValue<Widget>(prop('child'));

  // CupertinoCheckbox properties
  bool? get cupertinoCheckboxValue => extractBool(prop('value'));
  bool get cupertinoCheckboxTristate => extractBool(prop('tristate')) ?? false;
  ValueChanged<bool?>? get cupertinoCheckboxOnChanged =>
      extractNativeValue<ValueChanged<bool?>>(prop('on-changed'));
  Color? get cupertinoCheckboxActiveColor => extractColor(prop('active-color'));
  Color? get cupertinoCheckboxInactiveColor =>
      extractColor(prop('inactive-color'));
  Color? get cupertinoCheckboxCheckColor => extractColor(prop('check-color'));
  Color? get cupertinoCheckboxFocusColor => extractColor(prop('focus-color'));
}
