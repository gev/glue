import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:glue/ir.dart';
import 'package:glue/runtime.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Properties wrapper with lazy getters
/// Provides clean API for accessing widget properties without pre-computing everything
class Properties {
  final Map<String, Ir> _props;

  Properties(Map<String, Ir> props) : _props = props {
    print('Created Properties: ${props}');
  }

  /// Creates empty properties with all defaults
  Properties.empty() : _props = {};

  // Button properties
  String? get label => extractString(_props['label']);
  VoidCallback? onTap(Runtime runtime) =>
      extractVoidCallback(_props['on-tap'], runtime);
  VoidCallback? onLongPress(Runtime runtime) =>
      extractVoidCallback(_props['on-long-press'], runtime);
  ValueChanged<bool>? get onHover =>
      extractNativeValue<ValueChanged<bool>>(_props['on-hover']);
  ValueChanged<bool>? get onFocusChange =>
      extractNativeValue<ValueChanged<bool>>(_props['on-focus-change']);
  ButtonStyle? get buttonStyle =>
      extractNativeValue<ButtonStyle>(_props['style']);
  FocusNode? get focusNode =>
      extractNativeValue<FocusNode>(_props['focus-node']);
  bool get autofocus => extractBool(_props['autofocus']) ?? false;
  Clip get buttonClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.none;
  bool get disabled => extractBool(_props['disabled']) ?? false;

  // Text properties
  String? get content => extractString(_props['content']);
  Color? get color => extractColor(_props['color']);
  double? get size => extractDouble(_props['size']);
  FontWeight? get weight => extractFontWeight(_props['weight']);
  TextAlign? get align => extractTextAlign(_props['align']);
  TextOverflow? get overflow => extractTextOverflow(_props['overflow']);
  double? get textScaleFactor => extractDouble(_props['text-scale-factor']);
  int? get maxLines => extractInt(_props['max-lines']);
  String? get semanticsLabel => extractString(_props['semantics-label']);
  TextWidthBasis? get textWidthBasis =>
      extractTextWidthBasis(_props['text-width-basis']);
  TextHeightBehavior? get textHeightBehavior =>
      extractNativeValue<TextHeightBehavior>(_props['text-height-behavior']);
  bool? get softWrap => extractBool(_props['soft-wrap']);
  Locale? get locale => extractNativeValue<Locale>(_props['locale']);
  StrutStyle? get strutStyle =>
      extractNativeValue<StrutStyle>(_props['strut-style']);
  TextStyle? get textStyle =>
      extractNativeValue<TextStyle>(_props['text-style']);

  // Layout properties
  List<Widget> get children => extractChildren(_props['children']) ?? [];
  Widget? get child => extractChild(_props['child']);
  MainAxisAlignment get mainAlign =>
      extractMainAxisAlignment(_props['main-axis-align']) ??
      MainAxisAlignment.start;
  CrossAxisAlignment get crossAlign =>
      extractCrossAxisAlignment(_props['cross-axis-align']) ??
      CrossAxisAlignment.start;
  Axis get direction => extractAxis(_props['direction']) ?? Axis.vertical;
  double? get spacing => extractDouble(_props['spacing']);
  MainAxisSize get mainAxisSize =>
      extractMainAxisSize(_props['main-axis-size']) ?? MainAxisSize.max;
  TextDirection? get textDirection =>
      extractTextDirection(_props['text-direction']);
  VerticalDirection get verticalDirection =>
      extractVerticalDirection(_props['vertical-direction']) ??
      VerticalDirection.down;
  TextBaseline? get textBaseline =>
      extractTextBaseline(_props['text-baseline']);

  // Icon properties
  IconData? get icon => extractNativeValue<IconData>(_props['icon']);

  // FlutterLogo properties
  FlutterLogoStyle? get flutterLogoStyle =>
      extractFlutterLogoStyle(_props['style']);
  Duration? get duration => extractNativeValue<Duration>(_props['duration']);
  Curve? get curve => extractNativeValue<Curve>(_props['curve']);

  // Image properties
  ImageProvider? get imageProvider =>
      extractNativeValue<ImageProvider>(_props['image']);
  BoxFit? get boxFit => extractBoxFit(_props['fit']);
  BlendMode? get blendMode =>
      extractNativeValue<BlendMode>(_props['color-blend-mode']);
  ImageRepeat? get imageRepeat => extractImageRepeat(_props['repeat']);
  bool? get matchTextDirection => extractBool(_props['match-text-direction']);
  bool? get gaplessPlayback => extractBool(_props['gapless-playback']);
  bool? get excludeFromSemantics =>
      extractBool(_props['exclude-from-semantics']);
  FilterQuality? get filterQuality =>
      extractFilterQuality(_props['filter-quality']);
  int? get cacheWidth => extractInt(_props['cache-width']);
  int? get cacheHeight => extractInt(_props['cache-height']);

  // AppBar properties
  Widget? get title => extractNativeValue<Widget>(_props['title']);
  List<Widget>? get actions => extractChildren(_props['actions']);
  Color? get foregroundColor => extractColor(_props['foreground-color']);
  Color? get shadowColor => extractColor(_props['shadow-color']);
  Color? get surfaceTintColor => extractColor(_props['surface-tint-color']);
  bool? get centerTitle => extractBool(_props['center-title']);
  double? get titleSpacing => extractDouble(_props['title-spacing']);
  double? get toolbarOpacity => extractDouble(_props['toolbar-opacity']);
  double? get bottomOpacity => extractDouble(_props['bottom-opacity']);
  double? get toolbarHeight => extractDouble(_props['toolbar-height']);
  double? get leadingWidth => extractDouble(_props['leading-width']);
  bool? get primary => extractBool(_props['primary']);
  bool? get excludeHeaderSemantics =>
      extractBool(_props['exclude-header-semantics']);

  // Advanced AppBar properties
  bool? get automaticallyImplyLeading =>
      extractBool(_props['automatically-imply-leading']);
  bool? get automaticallyImplyActions =>
      extractBool(_props['automatically-imply-actions']);
  Widget? get flexibleSpace => extractChild(_props['flexible-space']);
  PreferredSizeWidget? get bottomAppBar =>
      extractNativeValue<PreferredSizeWidget>(_props['bottom']);
  double? get scrolledUnderElevation =>
      extractDouble(_props['scrolled-under-elevation']);
  ScrollNotificationPredicate? get notificationPredicate =>
      extractNativeValue<ScrollNotificationPredicate>(
        _props['notification-predicate'],
      );
  ShapeBorder? get shape => extractNativeValue<ShapeBorder>(_props['shape']);
  IconThemeData? get iconTheme =>
      extractNativeValue<IconThemeData>(_props['icon-theme']);
  IconThemeData? get actionsIconTheme =>
      extractNativeValue<IconThemeData>(_props['actions-icon-theme']);
  TextStyle? get titleTextStyle =>
      extractNativeValue<TextStyle>(_props['title-text-style']);
  TextStyle? get toolbarTextStyle =>
      extractNativeValue<TextStyle>(_props['toolbar-text-style']);
  SystemUiOverlayStyle? get systemOverlayStyle =>
      extractNativeValue<SystemUiOverlayStyle>(_props['system-overlay-style']);
  bool? get forceMaterialTransparency =>
      extractBool(_props['force-material-transparency']);
  bool? get useDefaultSemanticsOrder =>
      extractBool(_props['use-default-semantics-order']);
  EdgeInsetsGeometry? get actionsPadding =>
      extractEdgeInsets(_props['actions-padding']);
  bool? get animateColor => extractBool(_props['animate-color']);

  // Scaffold properties
  PreferredSizeWidget? get appBar =>
      extractNativeValue<PreferredSizeWidget>(_props['app-bar']);
  Widget? get body => extractChild(_props['body']);
  Widget? get floatingActionButton =>
      extractChild(_props['floating-action-button']);
  FloatingActionButtonLocation? get floatingActionButtonLocation =>
      extractFloatingActionButtonLocation(
        _props['floating-action-button-location'],
      );
  Widget? get floatingActionButtonAnimator =>
      extractChild(_props['floating-action-button-animator']);
  List<Widget>? get persistentFooterButtons =>
      extractChildren(_props['persistent-footer-buttons']);
  Widget? get drawer => extractChild(_props['drawer']);
  Widget? get endDrawer => extractChild(_props['end-drawer']);
  Widget? get bottomNavigationBar =>
      extractChild(_props['bottom-navigation-bar']);
  Widget? get bottomSheet => extractChild(_props['bottom-sheet']);
  bool? get resizeToAvoidBottomInset =>
      extractBool(_props['resize-to-avoid-bottom-inset']);
  bool? get extendBody => extractBool(_props['extend-body']);
  bool? get extendBodyBehindAppBar =>
      extractBool(_props['extend-body-behind-app-bar']);
  Color? get drawerScrimColor => extractColor(_props['drawer-scrim-color']);
  double? get drawerEdgeDragWidth =>
      extractDouble(_props['drawer-edge-drag-width']);
  bool? get drawerEnableOpenDragGesture =>
      extractBool(_props['drawer-enable-open-drag-gesture']);
  bool? get endDrawerEnableOpenDragGesture =>
      extractBool(_props['end-drawer-enable-open-drag-gesture']);
  String? get restorationId => extractString(_props['restoration-id']);

  // Card properties
  bool? get borderOnForeground => extractBool(_props['border-on-foreground']);
  bool? get semanticContainer => extractBool(_props['semantic-container']);

  // ListTile properties
  Widget? get subtitle => extractChild(_props['subtitle']);
  Widget? get trailing => extractChild(_props['trailing']);
  bool? get isThreeLine => extractBool(_props['is-three-line']);
  bool? get dense => extractBool(_props['dense']);
  VisualDensity? get visualDensity =>
      extractNativeValue<VisualDensity>(_props['visual-density']);
  ListTileStyle? get listTileStyle =>
      extractNativeValue<ListTileStyle>(_props['style']);
  Color? get selectedColor => extractColor(_props['selected-color']);
  Color? get iconColor => extractColor(_props['icon-color']);
  Color? get textColor => extractColor(_props['text-color']);
  TextStyle? get listTileTitleTextStyle =>
      extractNativeValue<TextStyle>(_props['title-text-style']);
  TextStyle? get subtitleTextStyle =>
      extractNativeValue<TextStyle>(_props['subtitle-text-style']);
  TextStyle? get leadingAndTrailingTextStyle =>
      extractNativeValue<TextStyle>(_props['leading-and-trailing-text-style']);
  EdgeInsetsGeometry? get contentPadding =>
      extractEdgeInsets(_props['content-padding']);
  bool? get enabled => extractBool(_props['enabled']);
  GestureTapCallback? get onTileTap =>
      extractNativeValue<GestureTapCallback>(_props['on-tap']);
  GestureLongPressCallback? get onTileLongPress =>
      extractNativeValue<GestureLongPressCallback>(_props['on-long-press']);
  MouseCursor? get mouseCursor =>
      extractNativeValue<MouseCursor>(_props['mouse-cursor']);
  bool? get selected => extractBool(_props['selected']);
  Color? get focusColor => extractColor(_props['focus-color']);
  Color? get hoverColor => extractColor(_props['hover-color']);
  Color? get splashColor => extractColor(_props['splash-color']);
  Color? get tileColor => extractColor(_props['tile-color']);
  Color? get selectedTileColor => extractColor(_props['selected-tile-color']);
  bool? get enableFeedback => extractBool(_props['enable-feedback']);
  double? get horizontalTitleGap =>
      extractDouble(_props['horizontal-title-gap']);
  double? get minVerticalPadding =>
      extractDouble(_props['min-vertical-padding']);
  double? get minLeadingWidth => extractDouble(_props['min-leading-width']);
  double? get minTileHeight => extractDouble(_props['min-tile-height']);
  ListTileTitleAlignment? get titleAlignment =>
      extractNativeValue<ListTileTitleAlignment>(_props['title-alignment']);

  // SnackBar properties
  Widget? get snackBarContent => extractChild(_props['content']);
  SnackBarBehavior? get snackBarBehavior =>
      extractNativeValue<SnackBarBehavior>(_props['behavior']);
  SnackBarAction? get snackBarAction =>
      extractNativeValue<SnackBarAction>(_props['action']);
  Duration? get snackBarDuration =>
      extractNativeValue<Duration>(_props['duration']);
  Animation<double>? get snackBarAnimation =>
      extractNativeValue<Animation<double>>(_props['animation']);
  VoidCallback? get onVisible =>
      extractNativeValue<VoidCallback>(_props['on-visible']);
  DismissDirection? get dismissDirection =>
      extractNativeValue<DismissDirection>(_props['dismiss-direction']);

  // TextField properties
  TextEditingController? get textEditingController =>
      extractNativeValue<TextEditingController>(_props['controller']);
  InputDecoration? get inputDecoration =>
      extractNativeValue<InputDecoration>(_props['decoration']);
  TextInputType? get keyboardType =>
      extractNativeValue<TextInputType>(_props['keyboard-type']);
  TextInputAction? get textInputAction =>
      extractNativeValue<TextInputAction>(_props['text-input-action']);
  TextCapitalization get textCapitalization =>
      extractNativeValue<TextCapitalization>(_props['text-capitalization']) ??
      TextCapitalization.none;
  TextAlignVertical? get textAlignVertical =>
      extractNativeValue<TextAlignVertical>(_props['text-align-vertical']);
  bool get readOnly => extractBool(_props['read-only']) ?? false;
  bool get textFieldAutofocus => extractBool(_props['autofocus']) ?? false;
  String get obscuringCharacter =>
      extractString(_props['obscuring-character']) ?? '•';
  bool get obscureText => extractBool(_props['obscure-text']) ?? false;
  bool get enableSuggestions =>
      extractBool(_props['enable-suggestions']) ?? true;
  int? get textFieldMaxLines => extractInt(_props['max-lines']);
  int? get minLines => extractInt(_props['min-lines']);
  bool get expands => extractBool(_props['expands']) ?? false;
  int? get maxLength => extractInt(_props['max-length']);
  MaxLengthEnforcement? get maxLengthEnforcement =>
      extractNativeValue<MaxLengthEnforcement>(
        _props['max-length-enforcement'],
      );
  ValueChanged<String>? get onTextChanged =>
      extractNativeValue<ValueChanged<String>>(_props['on-changed']);
  VoidCallback? get onEditingComplete =>
      extractNativeValue<VoidCallback>(_props['on-editing-complete']);
  ValueChanged<String>? get onSubmitted =>
      extractNativeValue<ValueChanged<String>>(_props['on-submitted']);
  List<TextInputFormatter>? get inputFormatters =>
      extractNativeValue<List<TextInputFormatter>>(_props['input-formatters']);
  bool? get textFieldEnabled => extractBool(_props['enabled']);
  double get cursorWidth => extractDouble(_props['cursor-width']) ?? 2.0;
  double? get cursorHeight => extractDouble(_props['cursor-height']);
  Radius? get cursorRadius =>
      extractNativeValue<Radius>(_props['cursor-radius']);
  Color? get cursorColor => extractColor(_props['cursor-color']);
  Color? get cursorErrorColor => extractColor(_props['cursor-error-color']);
  Brightness? get keyboardAppearance =>
      extractNativeValue<Brightness>(_props['keyboard-appearance']);
  EdgeInsets get textFieldScrollPadding =>
      extractNativeValue<EdgeInsets>(_props['scroll-padding']) ??
      const EdgeInsets.all(20.0);
  bool? get enableInteractiveSelection =>
      extractBool(_props['enable-interactive-selection']);
  bool? get selectAllOnFocus => extractBool(_props['select-all-on-focus']);
  TextSelectionControls? get selectionControls =>
      extractNativeValue<TextSelectionControls>(_props['selection-controls']);
  GestureTapCallback? get onTextFieldTap =>
      extractNativeValue<GestureTapCallback>(_props['on-tap']);
  bool get onTapAlwaysCalled =>
      extractBool(_props['on-tap-always-called']) ?? false;
  MouseCursor? get textFieldMouseCursor =>
      extractNativeValue<MouseCursor>(_props['mouse-cursor']);
  ScrollController? get textFieldScrollController =>
      extractNativeValue<ScrollController>(_props['scroll-controller']);
  ScrollPhysics? get scrollPhysics =>
      extractNativeValue<ScrollPhysics>(_props['scroll-physics']);
  Iterable<String>? get autofillHints =>
      extractNativeValue<Iterable<String>>(_props['autofill-hints']);
  String? get textFieldRestorationId => extractString(_props['restoration-id']);

  // FloatingActionButton properties
  String? get tooltip => extractString(_props['tooltip']);
  Object? get heroTag => extractNativeValue<Object>(_props['hero-tag']);
  double? get focusElevation => extractDouble(_props['focus-elevation']);
  double? get hoverElevation => extractDouble(_props['hover-elevation']);
  double? get highlightElevation =>
      extractDouble(_props['highlight-elevation']);
  double? get disabledElevation => extractDouble(_props['disabled-elevation']);
  bool? get mini => extractBool(_props['mini']);
  bool get isExtended => extractBool(_props['is-extended']) ?? false;
  MaterialTapTargetSize? get materialTapTargetSize =>
      extractNativeValue<MaterialTapTargetSize>(
        _props['material-tap-target-size'],
      );
  bool? get fabEnableFeedback => extractBool(_props['enable-feedback']);
  Alignment? get fabAlignment =>
      extractNativeValue<Alignment>(_props['alignment']);
  Offset? get fabOffset => extractNativeValue<Offset>(_props['offset']);

  // IconButton properties
  double get iconButtonIconSize => extractDouble(_props['icon-size']) ?? 24.0;
  VisualDensity? get iconButtonVisualDensity =>
      extractNativeValue<VisualDensity>(_props['visual-density']);
  EdgeInsetsGeometry get iconButtonPadding =>
      extractEdgeInsets(_props['padding']) ?? const EdgeInsets.all(8.0);
  AlignmentGeometry get iconButtonAlignment =>
      extractNativeValue<AlignmentGeometry>(_props['alignment']) ??
      Alignment.center;
  double? get splashRadius => extractDouble(_props['splash-radius']);
  bool? get iconButtonMini => extractBool(_props['mini']);
  Color? get highlightColor => extractColor(_props['highlight-color']);
  Color? get disabledColor => extractColor(_props['disabled-color']);

  // Checkbox properties
  bool? get checkboxValue => extractBool(_props['value']);
  bool get tristate => extractBool(_props['tristate']) ?? false;
  ValueChanged<bool?>? get onCheckboxChanged =>
      extractNativeValue<ValueChanged<bool?>>(_props['on-changed']);
  Color? get activeColor => extractColor(_props['active-color']);
  WidgetStateProperty<Color?>? get fillColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['fill-color']);
  Color? get checkColor => extractColor(_props['check-color']);
  OutlinedBorder? get checkboxShape =>
      extractNativeValue<OutlinedBorder>(_props['shape']);
  BorderSide? get checkboxSide =>
      extractNativeValue<BorderSide>(_props['side']);
  bool get isError => extractBool(_props['is-error']) ?? false;
  String? get checkboxSemanticLabel => extractString(_props['semantic-label']);
  WidgetStateProperty<Color?>? get overlayColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['overlay-color']);

  // Switch properties
  bool get switchValue => extractBool(_props['value']) ?? false;
  ValueChanged<bool>? get onSwitchChanged =>
      extractNativeValue<ValueChanged<bool>>(_props['on-changed']);
  Color? get activeThumbColor => extractColor(_props['active-thumb-color']);
  Color? get activeTrackColor => extractColor(_props['active-track-color']);
  Color? get inactiveThumbColor => extractColor(_props['inactive-thumb-color']);
  Color? get inactiveTrackColor => extractColor(_props['inactive-track-color']);
  ImageProvider<Object>? get activeThumbImage =>
      extractNativeValue<ImageProvider<Object>>(_props['active-thumb-image']);
  ImageErrorListener? get onActiveThumbImageError =>
      extractNativeValue<ImageErrorListener>(
        _props['on-active-thumb-image-error'],
      );
  ImageProvider<Object>? get inactiveThumbImage =>
      extractNativeValue<ImageProvider<Object>>(_props['inactive-thumb-image']);
  ImageErrorListener? get onInactiveThumbImageError =>
      extractNativeValue<ImageErrorListener>(
        _props['on-inactive-thumb-image-error'],
      );
  WidgetStateProperty<Color?>? get thumbColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['thumb-color']);
  WidgetStateProperty<Color?>? get trackColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['track-color']);
  WidgetStateProperty<Color?>? get trackOutlineColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        _props['track-outline-color'],
      );
  WidgetStateProperty<double?>? get trackOutlineWidth =>
      extractNativeValue<WidgetStateProperty<double?>>(
        _props['track-outline-width'],
      );
  WidgetStateProperty<Icon?>? get thumbIcon =>
      extractNativeValue<WidgetStateProperty<Icon?>>(_props['thumb-icon']);
  EdgeInsetsGeometry? get switchPadding => extractEdgeInsets(_props['padding']);

  // LinearProgressIndicator properties
  double? get progressValue => extractDouble(_props['value']);
  double? get progressMinHeight => extractDouble(_props['min-height']);
  String? get progressSemanticsLabel =>
      extractString(_props['semantics-label']);
  String? get progressSemanticsValue =>
      extractString(_props['semantics-value']);
  Animation<Color>? get valueColor =>
      extractNativeValue<Animation<Color>>(_props['value-color']);

  // Badge properties
  Widget? get badgeLabel => extractChild(_props['label']);
  bool? get isLabelVisible => extractBool(_props['is-label-visible']);
  bool? get largeSize => extractBool(_props['large-size']);
  Offset? get badgeOffset => extractNativeValue<Offset>(_props['offset']);
  bool? get showBadge => extractBool(_props['show-badge']);

  // Divider properties
  double? get dividerHeight => extractDouble(_props['height']);
  double? get dividerThickness => extractDouble(_props['thickness']);
  double? get dividerIndent => extractDouble(_props['indent']);
  double? get dividerEndIndent => extractDouble(_props['end-indent']);
  BorderRadiusGeometry? get dividerRadius =>
      extractNativeValue<BorderRadiusGeometry>(_props['radius']);

  // AlertDialog properties
  Widget? get alertDialogIcon => extractChild(_props['icon']);
  EdgeInsetsGeometry? get alertDialogIconPadding =>
      extractEdgeInsets(_props['icon-padding']);
  Color? get alertDialogIconColor => extractColor(_props['icon-color']);
  EdgeInsetsGeometry? get alertDialogTitlePadding =>
      extractEdgeInsets(_props['title-padding']);
  TextStyle? get alertDialogTitleTextStyle =>
      extractNativeValue<TextStyle>(_props['title-text-style']);
  EdgeInsetsGeometry? get alertDialogContentPadding =>
      extractEdgeInsets(_props['content-padding']);
  TextStyle? get alertDialogContentTextStyle =>
      extractNativeValue<TextStyle>(_props['content-text-style']);
  List<Widget>? get alertDialogActions => extractChildren(_props['actions']);
  EdgeInsetsGeometry? get alertDialogActionsPadding =>
      extractEdgeInsets(_props['actions-padding']);
  MainAxisAlignment? get alertDialogActionsAlignment =>
      extractMainAxisAlignment(_props['actions-alignment']);
  OverflowBarAlignment? get alertDialogActionsOverflowAlignment =>
      extractNativeValue<OverflowBarAlignment>(
        _props['actions-overflow-alignment'],
      );
  VerticalDirection? get alertDialogActionsOverflowDirection =>
      extractVerticalDirection(_props['actions-overflow-direction']);
  double? get alertDialogActionsOverflowButtonSpacing =>
      extractDouble(_props['actions-overflow-button-spacing']);
  EdgeInsetsGeometry? get alertDialogButtonPadding =>
      extractEdgeInsets(_props['button-padding']);
  String? get alertDialogSemanticLabel =>
      extractString(_props['semantic-label']);
  EdgeInsets? get alertDialogInsetPadding =>
      extractNativeValue<EdgeInsets>(_props['inset-padding']);
  bool get alertDialogScrollable => extractBool(_props['scrollable']) ?? false;

  // Chip properties
  Widget? get chipAvatar => extractChild(_props['avatar']);
  TextStyle? get chipLabelStyle =>
      extractNativeValue<TextStyle>(_props['label-style']);
  EdgeInsetsGeometry? get chipLabelPadding =>
      extractEdgeInsets(_props['label-padding']);
  Widget? get chipDeleteIcon => extractChild(_props['delete-icon']);
  VoidCallback? get chipOnDeleted =>
      extractNativeValue<VoidCallback>(_props['on-deleted']);
  Color? get chipDeleteIconColor => extractColor(_props['delete-icon-color']);
  String? get chipDeleteButtonTooltipMessage =>
      extractString(_props['delete-button-tooltip-message']);
  BorderSide? get chipSide => extractNativeValue<BorderSide>(_props['side']);
  OutlinedBorder? get chipShape =>
      extractNativeValue<OutlinedBorder>(_props['shape']);
  WidgetStateProperty<Color?>? get chipColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['color']);
  EdgeInsetsGeometry? get chipPadding => extractEdgeInsets(_props['padding']);
  BoxConstraints? get chipAvatarBoxConstraints =>
      extractNativeValue<BoxConstraints>(_props['avatar-box-constraints']);
  BoxConstraints? get chipDeleteIconBoxConstraints =>
      extractNativeValue<BoxConstraints>(_props['delete-icon-box-constraints']);

  // SegmentedButton properties
  Set<dynamic>? get segmentedSelected =>
      extractNativeValue<Set<dynamic>>(_props['selected']);
  List<Widget>? get segmentedSegments => extractChildren(_props['segments']);
  ValueChanged<Set<dynamic>>? get onSegmentedSelectionChanged =>
      extractNativeValue<ValueChanged<Set<dynamic>>>(
        _props['on-selection-changed'],
      );
  Set<dynamic>? get multiSelectionEnabledFor =>
      extractNativeValue<Set<dynamic>>(_props['multi-selection-enabled-for']);
  bool? get showSelectedIcon => extractBool(_props['show-selected-icon']);
  Color? get segmentedBackgroundColor =>
      extractColor(_props['background-color']);
  Color? get segmentedUnselectedColor =>
      extractColor(_props['unselected-color']);
  Color? get segmentedSelectedColor => extractColor(_props['selected-color']);
  Color? get segmentedDisabledColor => extractColor(_props['disabled-color']);
  Color? get segmentedShadowColor => extractColor(_props['shadow-color']);
  Color? get segmentedSurfaceTintColor =>
      extractColor(_props['surface-tint-color']);
  double? get segmentedElevation => extractDouble(_props['elevation']);

  // BottomNavigationBar properties
  List<BottomNavigationBarItem>? get bottomNavigationBarItems =>
      extractNativeValue<List<BottomNavigationBarItem>>(_props['items']);
  ValueChanged<int>? get onBottomNavigationBarTap =>
      extractNativeValue<ValueChanged<int>>(_props['on-tap']);
  int get bottomNavigationBarCurrentIndex =>
      extractInt(_props['current-index']) ?? 0;
  double get bottomNavigationBarElevation =>
      extractDouble(_props['elevation']) ?? 8.0;
  BottomNavigationBarType get bottomNavigationBarType =>
      extractNativeValue<BottomNavigationBarType>(_props['type']) ??
      BottomNavigationBarType.fixed;
  Color? get bottomNavigationBarFixedColor =>
      extractColor(_props['fixed-color']);
  Color? get bottomNavigationBarBackgroundColor =>
      extractColor(_props['background-color']);
  double get bottomNavigationBarIconSize =>
      extractDouble(_props['icon-size']) ?? 24.0;
  Color? get bottomNavigationBarSelectedItemColor =>
      extractColor(_props['selected-item-color']);
  Color? get bottomNavigationBarUnselectedItemColor =>
      extractColor(_props['unselected-item-color']);
  IconThemeData? get bottomNavigationBarSelectedIconTheme =>
      extractNativeValue<IconThemeData>(_props['selected-icon-theme']);
  IconThemeData? get bottomNavigationBarUnselectedIconTheme =>
      extractNativeValue<IconThemeData>(_props['unselected-icon-theme']);
  TextStyle? get bottomNavigationBarSelectedLabelStyle =>
      extractNativeValue<TextStyle>(_props['selected-label-style']);
  TextStyle? get bottomNavigationBarUnselectedLabelStyle =>
      extractNativeValue<TextStyle>(_props['unselected-label-style']);
  double get bottomNavigationBarSelectedFontSize =>
      extractDouble(_props['selected-font-size']) ?? 14.0;
  double get bottomNavigationBarUnselectedFontSize =>
      extractDouble(_props['unselected-font-size']) ?? 12.0;
  bool get bottomNavigationBarShowSelectedLabels =>
      extractBool(_props['show-selected-labels']) ?? true;
  bool get bottomNavigationBarShowUnselectedLabels =>
      extractBool(_props['show-unselected-labels']) ?? true;
  bool get bottomNavigationBarEnableFeedback =>
      extractBool(_props['enable-feedback']) ?? true;
  BottomNavigationBarLandscapeLayout? get bottomNavigationBarLandscapeLayout =>
      extractNativeValue<BottomNavigationBarLandscapeLayout>(
        _props['landscape-layout'],
      );

  // Drawer properties
  double get drawerWidth => extractDouble(_props['width']) ?? 304.0;
  Color? get drawerBackgroundColor => extractColor(_props['background-color']);
  Color? get drawerScrimColorProperty => extractColor(_props['scrim-color']);
  double get drawerElevation => extractDouble(_props['elevation']) ?? 16.0;
  Color? get drawerShadowColor => extractColor(_props['shadow-color']);
  Color? get drawerSurfaceTintColor =>
      extractColor(_props['surface-tint-color']);
  ShapeBorder? get drawerShape =>
      extractNativeValue<ShapeBorder>(_props['shape']);
  double get drawerSemanticLabel =>
      extractDouble(_props['semantic-label']) ?? 0.0;
  Clip get drawerClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.hardEdge;

  // Radio properties
  dynamic get radioValue => extractNativeValue<dynamic>(_props['value']);
  dynamic get radioGroupValue =>
      extractNativeValue<dynamic>(_props['group-value']);
  ValueChanged<dynamic>? get onRadioChanged =>
      extractNativeValue<ValueChanged<dynamic>>(_props['on-changed']);
  bool get toggleable => extractBool(_props['toggleable']) ?? false;
  WidgetStateProperty<Color?>? get radioFillColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['fill-color']);

  // Slider properties
  double get sliderValue => extractDouble(_props['value']) ?? 0.0;
  double? get sliderSecondaryTrackValue =>
      extractDouble(_props['secondary-track-value']);
  ValueChanged<double>? get onSliderChanged =>
      extractNativeValue<ValueChanged<double>>(_props['on-changed']);
  ValueChanged<double>? get onSliderChangeStart =>
      extractNativeValue<ValueChanged<double>>(_props['on-change-start']);
  ValueChanged<double>? get onSliderChangeEnd =>
      extractNativeValue<ValueChanged<double>>(_props['on-change-end']);
  double get sliderMin => extractDouble(_props['min']) ?? 0.0;
  double get sliderMax => extractDouble(_props['max']) ?? 1.0;
  int? get sliderDivisions => extractInt(_props['divisions']);
  String? get sliderLabel => extractString(_props['label']);
  SemanticFormatterCallback? get semanticFormatterCallback =>
      extractNativeValue<SemanticFormatterCallback>(
        _props['semantic-formatter-callback'],
      );
  SliderInteraction? get allowedInteraction =>
      extractNativeValue<SliderInteraction>(_props['allowed-interaction']);
  DragStartBehavior? get drawerDragStartBehavior =>
      extractDragStartBehavior(_props['drawer-drag-start-behavior']);
  FloatingActionButtonAnimator? get floatingActionButtonAnimatorProper =>
      extractNativeValue<FloatingActionButtonAnimator>(
        _props['floating-action-button-animator'],
      );

  // Container properties
  EdgeInsetsGeometry get padding =>
      extractEdgeInsets(_props['padding']) ?? EdgeInsets.zero;
  AlignmentGeometry? get alignment =>
      extractNativeValue<AlignmentGeometry>(_props['alignment']);
  double? get width => extractDouble(_props['width']);
  double? get height => extractDouble(_props['height']);
  BoxConstraints? get constraints =>
      extractNativeValue<BoxConstraints>(_props['constraints']);
  EdgeInsetsGeometry? get margin => extractEdgeInsets(_props['margin']);
  Decoration? get decoration =>
      extractNativeValue<Decoration>(_props['decoration']);
  Decoration? get foregroundDecoration =>
      extractNativeValue<Decoration>(_props['foreground-decoration']);
  Matrix4? get transform => extractNativeValue<Matrix4>(_props['transform']);
  AlignmentGeometry? get transformAlignment =>
      extractNativeValue<AlignmentGeometry>(_props['transform-alignment']);
  Clip get clipBehavior => extractClip(_props['clip-behavior']) ?? Clip.none;

  // DatePickerDialog properties
  DateTime? get datePickerInitialDate =>
      extractNativeValue<DateTime>(_props['initial-date']);
  DateTime? get datePickerFirstDate =>
      extractNativeValue<DateTime>(_props['first-date']);
  DateTime? get datePickerLastDate =>
      extractNativeValue<DateTime>(_props['last-date']);
  DateTime? get datePickerCurrentDate =>
      extractNativeValue<DateTime>(_props['current-date']);
  DatePickerEntryMode get datePickerInitialEntryMode =>
      extractNativeValue<DatePickerEntryMode>(_props['initial-entry-mode']) ??
      DatePickerEntryMode.calendar;
  SelectableDayPredicate? get datePickerSelectableDayPredicate =>
      extractNativeValue<SelectableDayPredicate>(
        _props['selectable-day-predicate'],
      );
  String? get datePickerCancelText => extractString(_props['cancel-text']);
  String? get datePickerConfirmText => extractString(_props['confirm-text']);
  String? get datePickerHelpText => extractString(_props['help-text']);
  DatePickerMode get datePickerInitialCalendarMode =>
      extractNativeValue<DatePickerMode>(_props['initial-calendar-mode']) ??
      DatePickerMode.day;
  String? get datePickerErrorFormatText =>
      extractString(_props['error-format-text']);
  String? get datePickerErrorInvalidText =>
      extractString(_props['error-invalid-text']);
  String? get datePickerFieldHintText =>
      extractString(_props['field-hint-text']);
  String? get datePickerFieldLabelText =>
      extractString(_props['field-label-text']);
  TextInputType? get datePickerKeyboardType =>
      extractNativeValue<TextInputType>(_props['keyboard-type']);
  String? get datePickerRestorationId =>
      extractString(_props['restoration-id']);
  ValueChanged<DatePickerEntryMode>? get datePickerOnDatePickerModeChange =>
      extractNativeValue<ValueChanged<DatePickerEntryMode>>(
        _props['on-date-picker-mode-change'],
      );
  Icon? get datePickerSwitchToInputEntryModeIcon =>
      extractNativeValue<Icon>(_props['switch-to-input-entry-mode-icon']);
  Icon? get datePickerSwitchToCalendarEntryModeIcon =>
      extractNativeValue<Icon>(_props['switch-to-calendar-entry-mode-icon']);
  EdgeInsets get datePickerInsetPadding =>
      extractNativeValue<EdgeInsets>(_props['inset-padding']) ??
      const EdgeInsets.symmetric(horizontal: 16.0, vertical: 24.0);
  CalendarDelegate<DateTime>? get datePickerCalendarDelegate =>
      extractNativeValue<CalendarDelegate<DateTime>>(
        _props['calendar-delegate'],
      );

  // TimePickerDialog properties
  TimeOfDay get timePickerInitialTime =>
      extractNativeValue<TimeOfDay>(_props['initial-time']) ?? TimeOfDay.now();
  String? get timePickerCancelText => extractString(_props['cancel-text']);
  String? get timePickerConfirmText => extractString(_props['confirm-text']);
  String? get timePickerHelpText => extractString(_props['help-text']);
  String? get timePickerErrorInvalidText =>
      extractString(_props['error-invalid-text']);
  String? get timePickerHourLabelText =>
      extractString(_props['hour-label-text']);
  String? get timePickerMinuteLabelText =>
      extractString(_props['minute-label-text']);
  String? get timePickerRestorationId =>
      extractString(_props['restoration-id']);
  TimePickerEntryMode get timePickerInitialEntryMode =>
      extractNativeValue<TimePickerEntryMode>(_props['initial-entry-mode']) ??
      TimePickerEntryMode.dial;
  Orientation? get timePickerOrientation =>
      extractNativeValue<Orientation>(_props['orientation']);
  EntryModeChangeCallback? get timePickerOnEntryModeChanged =>
      extractNativeValue<EntryModeChangeCallback>(
        _props['on-entry-mode-changed'],
      );
  Icon? get timePickerSwitchToInputEntryModeIcon =>
      extractNativeValue<Icon>(_props['switch-to-input-entry-mode-icon']);
  Icon? get timePickerSwitchToTimerEntryModeIcon =>
      extractNativeValue<Icon>(_props['switch-to-timer-entry-mode-icon']);
  bool get timePickerEmptyInitialInput =>
      extractBool(_props['empty-initial-input']) ?? false;

  // NavigationBar properties
  Duration? get navigationBarAnimationDuration =>
      extractNativeValue<Duration>(_props['animation-duration']);
  int get navigationBarSelectedIndex =>
      extractInt(_props['selected-index']) ?? 0;
  List<Widget>? get navigationBarDestinations =>
      extractChildren(_props['destinations']);
  ValueChanged<int>? get navigationBarOnDestinationSelected =>
      extractNativeValue<ValueChanged<int>>(_props['on-destination-selected']);
  Color? get navigationBarBackgroundColor =>
      extractColor(_props['background-color']);
  double? get navigationBarElevation => extractDouble(_props['elevation']);
  Color? get navigationBarShadowColor => extractColor(_props['shadow-color']);
  Color? get navigationBarSurfaceTintColor =>
      extractColor(_props['surface-tint-color']);
  Color? get navigationBarIndicatorColor =>
      extractColor(_props['indicator-color']);
  ShapeBorder? get navigationBarIndicatorShape =>
      extractNativeValue<ShapeBorder>(_props['indicator-shape']);
  double? get navigationBarHeight => extractDouble(_props['height']);
  NavigationDestinationLabelBehavior? get navigationBarLabelBehavior =>
      extractNativeValue<NavigationDestinationLabelBehavior>(
        _props['label-behavior'],
      );
  WidgetStateProperty<Color?>? get navigationBarOverlayColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['overlay-color']);
  WidgetStateProperty<TextStyle?>? get navigationBarLabelTextStyle =>
      extractNativeValue<WidgetStateProperty<TextStyle?>>(
        _props['label-text-style'],
      );
  EdgeInsetsGeometry? get navigationBarLabelPadding =>
      extractEdgeInsets(_props['label-padding']);
  bool get navigationBarMaintainBottomViewPadding =>
      extractBool(_props['maintain-bottom-view-padding']) ?? false;

  // TabBar properties
  List<Widget>? get tabBarTabs => extractChildren(_props['tabs']);
  TabController? get tabBarController =>
      extractNativeValue<TabController>(_props['controller']);
  bool get tabBarIsScrollable => extractBool(_props['is-scrollable']) ?? false;
  EdgeInsetsGeometry? get tabBarPadding => extractEdgeInsets(_props['padding']);
  Color? get tabBarIndicatorColor => extractColor(_props['indicator-color']);
  bool get tabBarAutomaticIndicatorColorAdjustment =>
      extractBool(_props['automatic-indicator-color-adjustment']) ?? true;
  double get tabBarIndicatorWeight =>
      extractDouble(_props['indicator-weight']) ?? 2.0;
  EdgeInsetsGeometry get tabBarIndicatorPadding =>
      extractEdgeInsets(_props['indicator-padding']) ?? EdgeInsets.zero;
  Decoration? get tabBarIndicator =>
      extractNativeValue<Decoration>(_props['indicator']);
  TabBarIndicatorSize? get tabBarIndicatorSize =>
      extractNativeValue<TabBarIndicatorSize>(_props['indicator-size']);
  Color? get tabBarDividerColor => extractColor(_props['divider-color']);
  double? get tabBarDividerHeight => extractDouble(_props['divider-height']);
  Color? get tabBarLabelColor => extractColor(_props['label-color']);
  TextStyle? get tabBarLabelStyle =>
      extractNativeValue<TextStyle>(_props['label-style']);
  EdgeInsetsGeometry? get tabBarLabelPadding =>
      extractEdgeInsets(_props['label-padding']);
  Color? get tabBarUnselectedLabelColor =>
      extractColor(_props['unselected-label-color']);
  TextStyle? get tabBarUnselectedLabelStyle =>
      extractNativeValue<TextStyle>(_props['unselected-label-style']);
  DragStartBehavior get tabBarDragStartBehavior =>
      extractDragStartBehavior(_props['drag-start-behavior']) ??
      DragStartBehavior.start;
  WidgetStateProperty<Color?>? get tabBarOverlayColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['overlay-color']);
  MouseCursor? get tabBarMouseCursor =>
      extractNativeValue<MouseCursor>(_props['mouse-cursor']);
  bool? get tabBarEnableFeedback => extractBool(_props['enable-feedback']);
  ValueChanged<int>? get tabBarOnTap =>
      extractNativeValue<ValueChanged<int>>(_props['on-tap']);
  ScrollPhysics? get tabBarPhysics =>
      extractNativeValue<ScrollPhysics>(_props['physics']);

  // MenuAnchor properties
  MenuController? get menuAnchorController =>
      extractNativeValue<MenuController>(_props['controller']);
  MenuStyle? get menuAnchorStyle =>
      extractNativeValue<MenuStyle>(_props['style']);
  Offset get menuAnchorAlignmentOffset =>
      extractNativeValue<Offset>(_props['alignment-offset']) ?? Offset.zero;
  EdgeInsetsGeometry? get menuAnchorReservedPadding =>
      extractEdgeInsets(_props['reserved-padding']);
  LayerLink? get menuAnchorLayerLink =>
      extractNativeValue<LayerLink>(_props['layer-link']);
  Clip get menuAnchorClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.hardEdge;
  bool get menuAnchorConsumeOutsideTap =>
      extractBool(_props['consume-outside-tap']) ?? false;
  VoidCallback? get menuAnchorOnOpen =>
      extractNativeValue<VoidCallback>(_props['on-open']);
  VoidCallback? get menuAnchorOnClose =>
      extractNativeValue<VoidCallback>(_props['on-close']);
  bool get menuAnchorCrossAxisUnconstrained =>
      extractBool(_props['cross-axis-unconstrained']) ?? true;
  bool get menuAnchorUseRootOverlay =>
      extractBool(_props['use-root-overlay']) ?? false;
  List<Widget>? get menuAnchorMenuChildren =>
      extractChildren(_props['menu-children']);
  MenuAnchorChildBuilder? get menuAnchorBuilder =>
      extractNativeValue<MenuAnchorChildBuilder>(_props['builder']);
}
