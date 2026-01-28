import 'package:flutter/cupertino.dart';
import 'package:flutter/cupertino.dart' as cupertino;
import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:flutter/material.dart' as material;
import 'package:flutter/services.dart';
import 'package:glue/ir.dart';
import 'package:glue/runtime.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Default hero tag for navigation bars
const _defaultHeroTag = '<default-hero-tag>';

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
  VoidCallback? onPress(Runtime runtime) =>
      extractVoidCallback(_props['on-press'], runtime);
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
      extractNativeValue<GestureTapCallback>(_props['on-press']);
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
  VoidCallback? onVisible(Runtime runtime) =>
      extractVoidCallback(_props['on-visible'], runtime);
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
  VoidCallback? onEditingComplete(Runtime runtime) =>
      extractVoidCallback(_props['on-editing-complete'], runtime);
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
      extractNativeValue<GestureTapCallback>(_props['on-press']);
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
  VoidCallback? chipOnDeleted(Runtime runtime) =>
      extractVoidCallback(_props['on-deleted'], runtime);
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
      extractNativeValue<ValueChanged<int>>(_props['on-press']);
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
      extractNativeValue<ValueChanged<int>>(_props['on-press']);
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

  // ExpansionTile properties
  Widget? get expansionTileLeading => extractChild(_props['leading']);
  Widget? get expansionTileTitle => extractChild(_props['title']);
  Widget? get expansionTileSubtitle => extractChild(_props['subtitle']);
  Widget? get expansionTileTrailing => extractChild(_props['trailing']);
  List<Widget>? get expansionTileChildren =>
      extractChildren(_props['children']);
  bool get expansionTileInitiallyExpanded =>
      extractBool(_props['initially-expanded']) ?? false;
  bool get expansionTileMaintainState =>
      extractBool(_props['maintain-state']) ?? false;
  EdgeInsetsGeometry get expansionTileTilePadding =>
      extractEdgeInsets(_props['tile-padding']) ??
      const EdgeInsets.symmetric(horizontal: 16.0, vertical: 8.0);
  Alignment get expansionTileExpandedAlignment =>
      extractNativeValue<Alignment>(_props['expanded-alignment']) ??
      Alignment.centerLeft;
  CrossAxisAlignment get expansionTileExpandedCrossAxisAlignment =>
      extractCrossAxisAlignment(_props['expanded-cross-axis-align']) ??
      CrossAxisAlignment.center;
  EdgeInsetsGeometry get expansionTileChildrenPadding =>
      extractEdgeInsets(_props['children-padding']) ??
      const EdgeInsets.symmetric(vertical: 8.0);
  Color? get expansionTileBackgroundColor =>
      extractColor(_props['background-color']);
  Color? get expansionTileCollapsedBackgroundColor =>
      extractColor(_props['collapsed-background-color']);
  Color? get expansionTileTextColor => extractColor(_props['text-color']);
  Color? get expansionTileCollapsedTextColor =>
      extractColor(_props['collapsed-text-color']);
  Color? get expansionTileIconColor => extractColor(_props['icon-color']);
  Color? get expansionTileCollapsedIconColor =>
      extractColor(_props['collapsed-icon-color']);
  ListTileControlAffinity get expansionTileControlAffinity =>
      extractNativeValue<ListTileControlAffinity>(_props['control-affinity']) ??
      ListTileControlAffinity.platform;
  ExpansionTileController? get expansionTileController =>
      extractNativeValue<ExpansionTileController>(_props['controller']);
  ValueChanged<bool>? get expansionTileOnExpansionChanged =>
      extractNativeValue<ValueChanged<bool>>(_props['on-expansion-changed']);

  // DataTable properties
  List<DataColumn>? get dataTableColumns =>
      extractNativeValue<List<DataColumn>>(_props['columns']);
  List<DataRow>? get dataTableRows =>
      extractNativeValue<List<DataRow>>(_props['rows']);
  int? get dataTableSortColumnIndex => extractInt(_props['sort-column-index']);
  bool get dataTableSortAscending =>
      extractBool(_props['sort-ascending']) ?? true;
  ValueSetter<bool?>? get dataTableOnSelectAll =>
      extractNativeValue<ValueSetter<bool?>>(_props['on-select-all']);
  WidgetStateProperty<Color?>? get dataTableDataRowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['data-row-color']);
  double get dataTableDataRowHeight =>
      extractDouble(_props['data-row-height']) ?? 48.0;
  TextStyle? get dataTableDataTextStyle =>
      extractNativeValue<TextStyle>(_props['data-text-style']);
  WidgetStateProperty<Color?>? get dataTableHeadingRowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        _props['heading-row-color'],
      );
  double get dataTableHeadingRowHeight =>
      extractDouble(_props['heading-row-height']) ?? 56.0;
  TextStyle? get dataTableHeadingTextStyle =>
      extractNativeValue<TextStyle>(_props['heading-text-style']);
  double get dataTableHorizontalMargin =>
      extractDouble(_props['horizontal-margin']) ?? 24.0;
  double get dataTableColumnSpacing =>
      extractDouble(_props['column-spacing']) ?? 56.0;
  bool get dataTableShowCheckboxColumn =>
      extractBool(_props['show-checkbox-column']) ?? true;
  bool get dataTableShowBottomBorder =>
      extractBool(_props['show-bottom-border']) ?? true;
  double get dataTableDividerThickness =>
      extractDouble(_props['divider-thickness']) ?? 1.0;
  double get dataTableCheckboxHorizontalMargin =>
      extractDouble(_props['checkbox-horizontal-margin']) ?? 24.0;
  TableBorder? get dataTableBorder =>
      extractNativeValue<TableBorder>(_props['border']);
  Clip get dataTableClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.none;

  // Tooltip properties
  String get tooltipMessage => extractString(_props['message']) ?? '';
  double get tooltipHeight => extractDouble(_props['height']) ?? 32.0;
  EdgeInsetsGeometry get tooltipPadding =>
      extractEdgeInsets(_props['padding']) ??
      const EdgeInsets.symmetric(horizontal: 16.0);
  EdgeInsetsGeometry? get tooltipMargin => extractEdgeInsets(_props['margin']);
  double get tooltipVerticalOffset =>
      extractDouble(_props['vertical-offset']) ?? 24.0;
  bool get tooltipPreferBelow => extractBool(_props['prefer-below']) ?? true;
  bool get tooltipExcludeFromSemantics =>
      extractBool(_props['exclude-from-semantics']) ?? false;
  Decoration? get tooltipDecoration =>
      extractNativeValue<Decoration>(_props['decoration']);
  TextStyle? get tooltipTextStyle =>
      extractNativeValue<TextStyle>(_props['text-style']);
  TextAlign get tooltipTextAlign =>
      extractTextAlign(_props['text-align']) ?? TextAlign.start;
  Duration get tooltipWaitDuration =>
      extractNativeValue<Duration>(_props['wait-duration']) ??
      const Duration(milliseconds: 0);
  Duration get tooltipShowDuration =>
      extractNativeValue<Duration>(_props['show-duration']) ??
      const Duration(milliseconds: 1500);
  TooltipTriggerMode get tooltipTriggerMode =>
      extractNativeValue<TooltipTriggerMode>(_props['trigger-mode']) ??
      TooltipTriggerMode.longPress;
  bool get tooltipEnableFeedback =>
      extractBool(_props['enable-feedback']) ?? true;
  TooltipTriggeredCallback? get tooltipOnTriggered =>
      extractNativeValue<TooltipTriggeredCallback>(_props['on-triggered']);
  InlineSpan? get tooltipRichMessage =>
      extractNativeValue<InlineSpan>(_props['rich-message']);

  // PopupMenuButton properties
  PopupMenuItemBuilder<Object>? get popupMenuItemBuilder =>
      extractNativeValue<PopupMenuItemBuilder<Object>>(_props['item-builder']);
  Object? get popupMenuInitialValue =>
      extractNativeValue<Object>(_props['initial-value']);
  PopupMenuItemSelected<Object>? get popupMenuOnSelected =>
      extractNativeValue<PopupMenuItemSelected<Object>>(_props['on-selected']);
  PopupMenuCanceled? get popupMenuOnCanceled =>
      extractNativeValue<PopupMenuCanceled>(_props['on-canceled']);
  String? get popupMenuTooltip => extractString(_props['tooltip']);
  double get popupMenuElevation => extractDouble(_props['elevation']) ?? 8.0;
  EdgeInsetsGeometry get popupMenuPadding =>
      extractEdgeInsets(_props['padding']) ?? const EdgeInsets.all(8.0);
  Widget? get popupMenuChild => extractChild(_props['child']);
  double? get popupMenuSplashRadius => extractDouble(_props['splash-radius']);
  Widget? get popupMenuIcon => extractChild(_props['icon']);
  double get popupMenuIconSize => extractDouble(_props['icon-size']) ?? 24.0;
  Offset get popupMenuOffset =>
      extractNativeValue<Offset>(_props['offset']) ?? Offset.zero;
  bool get popupMenuEnabled => extractBool(_props['enabled']) ?? true;
  ShapeBorder? get popupMenuShape =>
      extractNativeValue<ShapeBorder>(_props['shape']);
  Color? get popupMenuColor => extractColor(_props['color']);
  bool get popupMenuEnableFeedback =>
      extractBool(_props['enable-feedback']) ?? true;
  BoxConstraints? get popupMenuConstraints =>
      extractNativeValue<BoxConstraints>(_props['constraints']);
  PopupMenuPosition get popupMenuPosition =>
      extractNativeValue<PopupMenuPosition>(_props['position']) ??
      PopupMenuPosition.over;

  // DropdownButton properties
  List<DropdownMenuItem<Object>>? get dropdownItems =>
      extractNativeValue<List<DropdownMenuItem<Object>>>(_props['items']);
  DropdownButtonBuilder? get dropdownSelectedItemBuilder =>
      extractNativeValue<DropdownButtonBuilder>(
        _props['selected-item-builder'],
      );
  Object? get dropdownValue => extractNativeValue<Object>(_props['value']);
  Widget? get dropdownHint => extractChild(_props['hint']);
  Widget? get dropdownDisabledHint => extractChild(_props['disabled-hint']);
  ValueChanged<Object?>? get dropdownOnChanged =>
      extractNativeValue<ValueChanged<Object?>>(_props['on-changed']);
  GestureTapCallback? get dropdownOnTap =>
      extractNativeValue<GestureTapCallback>(_props['on-press']);
  int get dropdownElevation => extractInt(_props['elevation']) ?? 8;
  TextStyle? get dropdownStyle =>
      extractNativeValue<TextStyle>(_props['style']);
  Widget? get dropdownUnderline => extractChild(_props['underline']);
  Widget? get dropdownIcon => extractChild(_props['icon']);
  Color? get dropdownIconDisabledColor =>
      extractColor(_props['icon-disabled-color']);
  Color? get dropdownIconEnabledColor =>
      extractColor(_props['icon-enabled-color']);
  double get dropdownIconSize => extractDouble(_props['icon-size']) ?? 24.0;
  bool get dropdownIsDense => extractBool(_props['is-dense']) ?? false;
  bool get dropdownIsExpanded => extractBool(_props['is-expanded']) ?? false;
  double? get dropdownItemHeight => extractDouble(_props['item-height']);
  Color? get dropdownFocusColor => extractColor(_props['focus-color']);
  FocusNode? get dropdownFocusNode =>
      extractNativeValue<FocusNode>(_props['focus-node']);
  bool get dropdownAutofocus => extractBool(_props['autofocus']) ?? false;
  Color? get dropdownDropdownColor => extractColor(_props['dropdown-color']);
  double? get dropdownMenuMaxHeight => extractDouble(_props['menu-max-height']);
  bool get dropdownEnableFeedback =>
      extractBool(_props['enable-feedback']) ?? true;
  AlignmentGeometry get dropdownAlignment =>
      extractNativeValue<AlignmentGeometry>(_props['alignment']) ??
      Alignment.centerLeft;
  BorderRadius? get dropdownBorderRadius =>
      extractNativeValue<BorderRadius>(_props['border-radius']);
  EdgeInsetsGeometry? get dropdownPadding =>
      extractEdgeInsets(_props['padding']);

  // RefreshIndicator properties
  double get refreshDisplacement =>
      extractDouble(_props['displacement']) ?? 40.0;
  double get refreshEdgeOffset => extractDouble(_props['edge-offset']) ?? 0.0;
  cupertino.RefreshCallback? get cupertinoRefreshOnRefresh =>
      extractNativeValue<cupertino.RefreshCallback>(_props['on-refresh']);
  material.RefreshCallback? get materialRefreshOnRefresh =>
      extractNativeValue<material.RefreshCallback>(_props['on-refresh']);
  Color? get refreshColor => extractColor(_props['color']);
  Color? get refreshBackgroundColor => extractColor(_props['background-color']);
  ScrollNotificationPredicate get refreshNotificationPredicate =>
      extractNativeValue<ScrollNotificationPredicate>(
        _props['notification-predicate'],
      ) ??
      defaultScrollNotificationPredicate;
  String? get refreshSemanticsLabel => extractString(_props['semantics-label']);
  String? get refreshSemanticsValue => extractString(_props['semantics-value']);
  double get refreshStrokeWidth => extractDouble(_props['stroke-width']) ?? 2.0;
  RefreshIndicatorTriggerMode get refreshTriggerMode =>
      extractNativeValue<RefreshIndicatorTriggerMode>(_props['trigger-mode']) ??
      RefreshIndicatorTriggerMode.onEdge;

  // CircularProgressIndicator properties
  double? get circularProgressValue => extractDouble(_props['value']);
  Color? get circularProgressBackgroundColor =>
      extractColor(_props['background-color']);
  double get circularProgressStrokeWidth =>
      extractDouble(_props['stroke-width']) ?? 4.0;
  double get circularProgressStrokeAlign =>
      extractDouble(_props['stroke-align']) ?? 0.0;
  StrokeCap get circularProgressStrokeCap =>
      extractNativeValue<StrokeCap>(_props['stroke-cap']) ?? StrokeCap.round;
  String? get circularProgressSemanticsLabel =>
      extractString(_props['semantics-label']);
  String? get circularProgressSemanticsValue =>
      extractString(_props['semantics-value']);

  // Stepper properties
  List<Step>? get stepperSteps =>
      extractNativeValue<List<Step>>(_props['steps']);
  int get stepperCurrentStep => extractInt(_props['current-step']) ?? 0;
  ValueChanged<int>? get stepperOnStepTapped =>
      extractNativeValue<ValueChanged<int>>(_props['on-step-tapped']);
  VoidCallback? get stepperOnStepContinue =>
      extractNativeValue<VoidCallback>(_props['on-step-continue']);
  VoidCallback? get stepperOnStepCancel =>
      extractNativeValue<VoidCallback>(_props['on-step-cancel']);
  ControlsWidgetBuilder? get stepperControlsBuilder =>
      extractNativeValue<ControlsWidgetBuilder>(_props['controls-builder']);
  StepperType get stepperType =>
      extractNativeValue<StepperType>(_props['type']) ?? StepperType.vertical;
  ScrollPhysics? get stepperPhysics =>
      extractNativeValue<ScrollPhysics>(_props['physics']);
  double get stepperElevation => extractDouble(_props['elevation']) ?? 0.0;
  EdgeInsetsGeometry get stepperMargin =>
      extractEdgeInsets(_props['margin']) ?? EdgeInsets.zero;
  WidgetStateProperty<Color>? get stepperConnectorColor =>
      extractNativeValue<WidgetStateProperty<Color>>(_props['connector-color']);
  double get stepperConnectorThickness =>
      extractDouble(_props['connector-thickness']) ?? 1.0;
  StepIconBuilder? get stepperStepIconBuilder =>
      extractNativeValue<StepIconBuilder>(_props['step-icon-builder']);
  double get stepperStepContent => extractDouble(_props['step-content']) ?? 0.0;

  // ExpansionPanelList properties
  List<ExpansionPanel>? get expansionPanelListChildren =>
      extractNativeValue<List<ExpansionPanel>>(_props['children']);
  ExpansionPanelCallback? get expansionPanelListExpansionCallback =>
      extractNativeValue<ExpansionPanelCallback>(_props['expansion-callback']);
  Duration get expansionPanelListAnimationDuration =>
      extractNativeValue<Duration>(_props['animation-duration']) ??
      const Duration(milliseconds: 200);
  double get expansionPanelListElevation =>
      extractDouble(_props['elevation']) ?? 2.0;
  double get expansionPanelListMaterialGapSize =>
      extractDouble(_props['material-gap-size']) ?? 16.0;
  Color? get expansionPanelListDividerColor =>
      extractColor(_props['divider-color']);
  Color? get expansionPanelListExpandIconColor =>
      extractColor(_props['expand-icon-color']);

  // TabBarView properties
  List<Widget>? get tabBarViewChildren => extractChildren(_props['children']);
  TabController? get tabBarViewController =>
      extractNativeValue<TabController>(_props['controller']);
  ScrollPhysics? get tabBarViewPhysics =>
      extractNativeValue<ScrollPhysics>(_props['physics']);
  DragStartBehavior get tabBarViewDragStartBehavior =>
      extractDragStartBehavior(_props['drag-start-behavior']) ??
      DragStartBehavior.start;
  double get tabBarViewViewportFraction =>
      extractDouble(_props['viewport-fraction']) ?? 1.0;

  // BottomSheet properties
  AnimationController? get bottomSheetAnimationController =>
      extractNativeValue<AnimationController>(_props['animation-controller']);
  bool get bottomSheetEnableDrag => extractBool(_props['enable-drag']) ?? true;
  bool? get bottomSheetShowDragHandle =>
      extractBool(_props['show-drag-handle']);
  Color? get bottomSheetDragHandleColor =>
      extractColor(_props['drag-handle-color']);
  Size? get bottomSheetDragHandleSize =>
      extractNativeValue<Size>(_props['drag-handle-size']);
  BottomSheetDragStartHandler? get bottomSheetOnDragStart =>
      extractNativeValue<BottomSheetDragStartHandler>(_props['on-drag-start']);
  BottomSheetDragEndHandler? get bottomSheetOnDragEnd =>
      extractNativeValue<BottomSheetDragEndHandler>(_props['on-drag-end']);
  VoidCallback get bottomSheetOnClosing =>
      extractNativeValue<VoidCallback>(_props['on-closing']) ?? () {};
  WidgetBuilder get bottomSheetBuilder =>
      extractNativeValue<WidgetBuilder>(_props['builder']) ??
      (_) => const SizedBox();

  // SearchBar properties
  SearchController? get searchBarController =>
      extractNativeValue<SearchController>(_props['controller']);
  FocusNode? get searchBarFocusNode =>
      extractNativeValue<FocusNode>(_props['focus-node']);
  String? get searchBarHintText => extractString(_props['hint-text']);
  Widget? get searchBarLeading => extractChild(_props['leading']);
  Iterable<Widget>? get searchBarTrailing =>
      extractChildren(_props['trailing']);
  VoidCallback? get searchBarOnTap =>
      extractNativeValue<VoidCallback>(_props['on-press']);
  ValueChanged<String>? get searchBarOnChanged =>
      extractNativeValue<ValueChanged<String>>(_props['on-changed']);
  ValueChanged<String>? get searchBarOnSubmitted =>
      extractNativeValue<ValueChanged<String>>(_props['on-submitted']);
  BoxConstraints? get searchBarConstraints =>
      extractNativeValue<BoxConstraints>(_props['constraints']);
  WidgetStateProperty<double?>? get searchBarElevation =>
      extractNativeValue<WidgetStateProperty<double?>>(_props['elevation']);
  WidgetStateProperty<Color?>? get searchBarBackgroundColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        _props['background-color'],
      );
  WidgetStateProperty<Color?>? get searchBarShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['shadow-color']);
  WidgetStateProperty<Color?>? get searchBarSurfaceTintColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        _props['surface-tint-color'],
      );
  WidgetStateProperty<Color?>? get searchBarOverlayColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['overlay-color']);
  WidgetStateProperty<BorderSide?>? get searchBarSide =>
      extractNativeValue<WidgetStateProperty<BorderSide?>>(_props['side']);
  WidgetStateProperty<OutlinedBorder?>? get searchBarShape =>
      extractNativeValue<WidgetStateProperty<OutlinedBorder?>>(_props['shape']);
  EdgeInsetsGeometry? get searchBarPadding =>
      extractEdgeInsets(_props['padding']);
  TextStyle? get searchBarTextStyle =>
      extractNativeValue<TextStyle>(_props['text-style']);
  TextStyle? get searchBarHintStyle =>
      extractNativeValue<TextStyle>(_props['hint-style']);
  TextCapitalization get searchBarTextCapitalization =>
      extractNativeValue<TextCapitalization>(_props['text-capitalization']) ??
      TextCapitalization.none;
  TextInputType? get searchBarKeyboardType =>
      extractNativeValue<TextInputType>(_props['keyboard-type']);
  Widget Function(Iterable<Widget> suggestions)? get searchBarViewBuilder =>
      extractNativeValue<Widget Function(Iterable<Widget> suggestions)>(
        _props['view-builder'],
      );
  BoxConstraints? get searchBarViewConstraints =>
      extractNativeValue<BoxConstraints>(_props['view-constraints']);
  double? get searchBarViewElevation => extractDouble(_props['view-elevation']);
  Color? get searchBarViewBackgroundColor =>
      extractColor(_props['view-background-color']);
  Color? get searchBarViewShadowColor =>
      extractColor(_props['view-shadow-color']);
  Color? get searchBarViewSurfaceTintColor =>
      extractColor(_props['view-surface-tint-color']);
  OutlinedBorder? get searchBarViewShape =>
      extractNativeValue<OutlinedBorder>(_props['view-shape']);
  BorderSide? get searchBarViewSide =>
      extractNativeValue<BorderSide>(_props['view-side']);
  EdgeInsetsGeometry? get searchBarViewPadding =>
      extractEdgeInsets(_props['view-padding']);
  Widget? get searchBarViewLeading => extractChild(_props['view-leading']);
  Iterable<Widget>? get searchBarViewTrailing =>
      extractChildren(_props['view-trailing']);
  String? get searchBarViewHintText => extractString(_props['view-hint-text']);
  TextStyle? get searchBarViewHintStyle =>
      extractNativeValue<TextStyle>(_props['view-hint-style']);
  TapRegionCallback? get searchBarOnTapOutside =>
      extractNativeValue<TapRegionCallback>(_props['on-tap-outside']);

  // SearchAnchor properties
  SearchController? get searchAnchorSearchController =>
      extractNativeValue<SearchController>(_props['search-controller']);
  SearchAnchorChildBuilder? get searchAnchorViewBuilder =>
      extractNativeValue<SearchAnchorChildBuilder>(_props['view-builder']);
  WidgetBuilder? get searchAnchorBuilder =>
      extractNativeValue<WidgetBuilder>(_props['builder']);
  BoxConstraints? get searchAnchorViewConstraints =>
      extractNativeValue<BoxConstraints>(_props['view-constraints']);
  double? get searchAnchorViewElevation =>
      extractDouble(_props['view-elevation']);
  Color? get searchAnchorViewBackgroundColor =>
      extractColor(_props['view-background-color']);
  Color? get searchAnchorViewShadowColor =>
      extractColor(_props['view-shadow-color']);
  Color? get searchAnchorViewSurfaceTintColor =>
      extractColor(_props['view-surface-tint-color']);
  OutlinedBorder? get searchAnchorViewShape =>
      extractNativeValue<OutlinedBorder>(_props['view-shape']);
  BorderSide? get searchAnchorViewSide =>
      extractNativeValue<BorderSide>(_props['view-side']);
  EdgeInsetsGeometry? get searchAnchorViewPadding =>
      extractEdgeInsets(_props['view-padding']);
  Widget? get searchAnchorViewLeading => extractChild(_props['view-leading']);
  Iterable<Widget>? get searchAnchorViewTrailing =>
      extractChildren(_props['view-trailing']);
  String? get searchAnchorViewHintText =>
      extractString(_props['view-hint-text']);
  TextStyle? get searchAnchorViewHintStyle =>
      extractNativeValue<TextStyle>(_props['view-hint-style']);
  bool get searchAnchorIsFullScreen =>
      extractBool(_props['is-full-screen']) ?? false;
  Color? get searchAnchorDividerColor => extractColor(_props['divider-color']);

  // InputChip properties
  bool get inputChipSelected => extractBool(_props['selected']) ?? false;
  bool get inputChipIsEnabled => extractBool(_props['is-enabled']) ?? true;
  Widget? get inputChipLabel => extractChild(_props['label']);
  TextStyle? get inputChipLabelStyle =>
      extractNativeValue<TextStyle>(_props['label-style']);
  EdgeInsetsGeometry? get inputChipLabelPadding =>
      extractEdgeInsets(_props['label-padding']);
  Widget? get inputChipDeleteIcon => extractChild(_props['delete-icon']);
  VoidCallback? inputChipOnDeleted(Runtime runtime) =>
      extractVoidCallback(_props['on-deleted'], runtime);
  Color? get inputChipDeleteIconColor =>
      extractColor(_props['delete-icon-color']);
  String? get inputChipDeleteButtonTooltipMessage =>
      extractString(_props['delete-button-tooltip-message']);
  ValueChanged<bool>? get inputChipOnSelected =>
      extractNativeValue<ValueChanged<bool>>(_props['on-selected']);
  VoidCallback? inputChipOnPressed(Runtime runtime) =>
      extractVoidCallback(_props['on-pressed'], runtime);
  double? get inputChipPressElevation =>
      extractDouble(_props['press-elevation']);
  Widget? get inputChipAvatar => extractChild(_props['avatar']);
  BoxConstraints? get inputChipAvatarBoxConstraints =>
      extractNativeValue<BoxConstraints>(_props['avatar-box-constraints']);
  ShapeBorder? get inputChipAvatarBorderProperty =>
      extractNativeValue<ShapeBorder>(_props['avatar-border']);
  BorderSide? get inputChipSide =>
      extractNativeValue<BorderSide>(_props['side']);
  OutlinedBorder? get inputChipShape =>
      extractNativeValue<OutlinedBorder>(_props['shape']);
  Clip get inputChipClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.none;
  FocusNode? get inputChipFocusNode =>
      extractNativeValue<FocusNode>(_props['focus-node']);
  bool get inputChipAutofocus => extractBool(_props['autofocus']) ?? false;
  Color? get inputChipBackgroundColor =>
      extractColor(_props['background-color']);
  Color? get inputChipDisabledColor => extractColor(_props['disabled-color']);
  Color? get inputChipSelectedColor => extractColor(_props['selected-color']);
  Color? get inputChipCheckmarkColor => extractColor(_props['checkmark-color']);
  bool? get inputChipShowCheckmark => extractBool(_props['show-checkmark']);
  WidgetStateProperty<Color?>? get inputChipColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['color']);
  WidgetStateProperty<Color?>? get inputChipSurfaceTintColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        _props['surface-tint-color'],
      );
  WidgetStateProperty<double?>? get inputChipElevation =>
      extractNativeValue<WidgetStateProperty<double?>>(_props['elevation']);
  WidgetStateProperty<Color?>? get inputChipShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['shadow-color']);
  WidgetStateProperty<Color?>? get inputChipSelectedShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        _props['selected-shadow-color'],
      );
  // FilterChip properties
  bool get filterChipSelected => extractBool(_props['selected']) ?? false;
  Widget? get filterChipLabel => extractChild(_props['label']);
  TextStyle? get filterChipLabelStyle =>
      extractNativeValue<TextStyle>(_props['label-style']);
  EdgeInsetsGeometry? get filterChipLabelPadding =>
      extractEdgeInsets(_props['label-padding']);
  Widget? get filterChipAvatar => extractChild(_props['avatar']);
  BoxConstraints? get filterChipAvatarBoxConstraints =>
      extractNativeValue<BoxConstraints>(_props['avatar-box-constraints']);
  ShapeBorder? get filterChipAvatarBorder =>
      extractNativeValue<ShapeBorder>(_props['avatar-border']);
  Widget? get filterChipDeleteIcon => extractChild(_props['delete-icon']);
  VoidCallback? get filterChipOnDeleted =>
      extractNativeValue<VoidCallback>(_props['on-deleted']);
  Color? get filterChipDeleteIconColor =>
      extractColor(_props['delete-icon-color']);
  String? get filterChipDeleteButtonTooltipMessage =>
      extractString(_props['delete-button-tooltip-message']);
  ValueChanged<bool>? get filterChipOnSelected =>
      extractNativeValue<ValueChanged<bool>>(_props['on-selected']);
  VoidCallback? get filterChipOnPressed =>
      extractNativeValue<VoidCallback>(_props['on-pressed']);
  double? get filterChipPressElevation =>
      extractDouble(_props['press-elevation']);
  BorderSide? get filterChipSide =>
      extractNativeValue<BorderSide>(_props['side']);
  OutlinedBorder? get filterChipShape =>
      extractNativeValue<OutlinedBorder>(_props['shape']);
  Clip get filterChipClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.none;
  FocusNode? get filterChipFocusNode =>
      extractNativeValue<FocusNode>(_props['focus-node']);
  bool get filterChipAutofocus => extractBool(_props['autofocus']) ?? false;
  Color? get filterChipBackgroundColor =>
      extractColor(_props['background-color']);
  Color? get filterChipDisabledColor => extractColor(_props['disabled-color']);
  Color? get filterChipSelectedColor => extractColor(_props['selected-color']);
  Color? get filterChipCheckmarkColor =>
      extractColor(_props['checkmark-color']);
  bool? get filterChipShowCheckmark => extractBool(_props['show-checkmark']);
  WidgetStateProperty<Color?>? get filterChipColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['color']);
  WidgetStateProperty<Color?>? get filterChipSurfaceTintColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        _props['surface-tint-color'],
      );
  WidgetStateProperty<double?>? get filterChipElevation =>
      extractNativeValue<WidgetStateProperty<double?>>(_props['elevation']);
  WidgetStateProperty<Color?>? get filterChipShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['shadow-color']);
  WidgetStateProperty<Color?>? get filterChipSelectedShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        _props['selected-shadow-color'],
      );
  // ChoiceChip properties
  bool get choiceChipSelected => extractBool(_props['selected']) ?? false;
  Widget? get choiceChipLabel => extractChild(_props['label']);
  TextStyle? get choiceChipLabelStyle =>
      extractNativeValue<TextStyle>(_props['label-style']);
  EdgeInsetsGeometry? get choiceChipLabelPadding =>
      extractEdgeInsets(_props['label-padding']);
  Widget? get choiceChipAvatar => extractChild(_props['avatar']);
  BoxConstraints? get choiceChipAvatarBoxConstraints =>
      extractNativeValue<BoxConstraints>(_props['avatar-box-constraints']);
  ShapeBorder? get choiceChipAvatarBorder =>
      extractNativeValue<ShapeBorder>(_props['avatar-border']);
  ValueChanged<bool>? get choiceChipOnSelected =>
      extractNativeValue<ValueChanged<bool>>(_props['on-selected']);
  double? get choiceChipPressElevation =>
      extractDouble(_props['press-elevation']);
  BorderSide? get choiceChipSide =>
      extractNativeValue<BorderSide>(_props['side']);
  OutlinedBorder? get choiceChipShape =>
      extractNativeValue<OutlinedBorder>(_props['shape']);
  Clip get choiceChipClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.none;
  FocusNode? get choiceChipFocusNode =>
      extractNativeValue<FocusNode>(_props['focus-node']);
  bool get choiceChipAutofocus => extractBool(_props['autofocus']) ?? false;
  Color? get choiceChipBackgroundColor =>
      extractColor(_props['background-color']);
  Color? get choiceChipDisabledColor => extractColor(_props['disabled-color']);
  Color? get choiceChipSelectedColor => extractColor(_props['selected-color']);
  Color? get choiceChipCheckmarkColor =>
      extractColor(_props['checkmark-color']);
  bool? get choiceChipShowCheckmark => extractBool(_props['show-checkmark']);
  WidgetStateProperty<Color?>? get choiceChipColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['color']);
  WidgetStateProperty<Color?>? get choiceChipSurfaceTintColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        _props['surface-tint-color'],
      );
  WidgetStateProperty<double?>? get choiceChipElevation =>
      extractNativeValue<WidgetStateProperty<double?>>(_props['elevation']);
  WidgetStateProperty<Color?>? get choiceChipShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['shadow-color']);
  WidgetStateProperty<Color?>? get choiceChipSelectedShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        _props['selected-shadow-color'],
      );
  // ActionChip properties
  Widget? get actionChipLabel => extractChild(_props['label']);
  TextStyle? get actionChipLabelStyle =>
      extractNativeValue<TextStyle>(_props['label-style']);
  EdgeInsetsGeometry? get actionChipLabelPadding =>
      extractEdgeInsets(_props['label-padding']);
  Widget? get actionChipAvatar => extractChild(_props['avatar']);
  BoxConstraints? get actionChipAvatarBoxConstraints =>
      extractNativeValue<BoxConstraints>(_props['avatar-box-constraints']);
  ShapeBorder? get actionChipAvatarBorder =>
      extractNativeValue<ShapeBorder>(_props['avatar-border']);
  VoidCallback? get actionChipOnPressed =>
      extractNativeValue<VoidCallback>(_props['on-pressed']);
  double? get actionChipPressElevation =>
      extractDouble(_props['press-elevation']);
  BorderSide? get actionChipSide =>
      extractNativeValue<BorderSide>(_props['side']);
  OutlinedBorder? get actionChipShape =>
      extractNativeValue<OutlinedBorder>(_props['shape']);
  Clip get actionChipClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.none;
  FocusNode? get actionChipFocusNode =>
      extractNativeValue<FocusNode>(_props['focus-node']);
  bool get actionChipAutofocus => extractBool(_props['autofocus']) ?? false;
  Color? get actionChipBackgroundColor =>
      extractColor(_props['background-color']);
  Color? get actionChipDisabledColor => extractColor(_props['disabled-color']);
  WidgetStateProperty<double?>? get actionChipElevation =>
      extractNativeValue<WidgetStateProperty<double?>>(_props['elevation']);
  WidgetStateProperty<Color?>? get actionChipShadowColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(_props['shadow-color']);
  WidgetStateProperty<Color?>? get actionChipSurfaceTintColor =>
      extractNativeValue<WidgetStateProperty<Color?>>(
        _props['surface-tint-color'],
      );

  // BottomAppBar properties
  Color? get bottomAppBarColor => extractColor(_props['color']);
  double get bottomAppBarElevation => extractDouble(_props['elevation']) ?? 8.0;
  ShapeBorder? get bottomAppBarShape =>
      extractNativeValue<ShapeBorder>(_props['shape']);
  Clip get bottomAppBarClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.none;
  double get bottomAppBarNotchMargin =>
      extractDouble(_props['notch-margin']) ?? 4.0;
  double? get bottomAppBarHeight => extractDouble(_props['height']);
  EdgeInsetsGeometry? get bottomAppBarPadding =>
      extractEdgeInsets(_props['padding']);
  Color? get bottomAppBarSurfaceTintColor =>
      extractColor(_props['surface-tint-color']);
  Color? get bottomAppBarShadowColor => extractColor(_props['shadow-color']);
  Widget? get bottomAppBarChild => extractChild(_props['child']);

  // NavigationDrawer properties
  Color? get navigationDrawerBackgroundColor =>
      extractColor(_props['background-color']);
  double get navigationDrawerElevation =>
      extractDouble(_props['elevation']) ?? 1.0;
  Color? get navigationDrawerShadowColor =>
      extractColor(_props['shadow-color']);
  Color? get navigationDrawerSurfaceTintColor =>
      extractColor(_props['surface-tint-color']);
  Color? get navigationDrawerIndicatorColor =>
      extractColor(_props['indicator-color']);
  ShapeBorder? get navigationDrawerIndicatorShape =>
      extractNativeValue<ShapeBorder>(_props['indicator-shape']);
  int get navigationDrawerSelectedIndex =>
      extractInt(_props['selected-index']) ?? 0;
  ValueChanged<int>? get navigationDrawerOnDestinationSelected =>
      extractNativeValue<ValueChanged<int>>(_props['on-destination-selected']);
  List<Widget>? get navigationDrawerChildren =>
      extractChildren(_props['children']);
  EdgeInsetsGeometry? get navigationDrawerTilePadding =>
      extractEdgeInsets(_props['tile-padding']);

  // DrawerHeader properties
  Decoration? get drawerHeaderDecoration =>
      extractNativeValue<Decoration>(_props['decoration']);
  EdgeInsetsGeometry? get drawerHeaderMargin =>
      extractEdgeInsets(_props['margin']);
  EdgeInsetsGeometry? get drawerHeaderPadding =>
      extractEdgeInsets(_props['padding']);
  Duration? get drawerHeaderDuration =>
      extractNativeValue<Duration>(_props['duration']);
  Curve? get drawerHeaderCurve => extractNativeValue<Curve>(_props['curve']);
  Widget? get drawerHeaderChild => extractChild(_props['child']);

  // UserAccountsDrawerHeader properties
  Decoration? get userAccountsDrawerHeaderDecoration =>
      extractNativeValue<Decoration>(_props['decoration']);
  EdgeInsetsGeometry? get userAccountsDrawerHeaderMargin =>
      extractEdgeInsets(_props['margin']);
  EdgeInsetsGeometry? get userAccountsDrawerHeaderPadding =>
      extractEdgeInsets(_props['padding']);
  Widget? get userAccountsDrawerHeaderCurrentAccountPicture =>
      extractChild(_props['current-account-picture']);
  List<Widget>? get userAccountsDrawerHeaderOtherAccountsPictures =>
      extractChildren(_props['other-accounts-pictures']);
  Widget? get userAccountsDrawerHeaderAccountName =>
      extractChild(_props['account-name']);
  Widget? get userAccountsDrawerHeaderAccountEmail =>
      extractChild(_props['account-email']);
  VoidCallback? get userAccountsDrawerHeaderOnDetailsPressed =>
      extractNativeValue<VoidCallback>(_props['on-details-pressed']);
  Color? get userAccountsDrawerHeaderArrowColor =>
      extractColor(_props['arrow-color']);

  // ListView properties
  Axis get listViewScrollDirection =>
      extractAxis(_props['scroll-direction']) ?? Axis.vertical;
  bool get listViewReverse => extractBool(_props['reverse']) ?? false;
  ScrollController? get listViewController =>
      extractNativeValue<ScrollController>(_props['controller']);
  bool get listViewPrimary => extractBool(_props['primary']) ?? false;
  ScrollPhysics? get listViewPhysics =>
      extractNativeValue<ScrollPhysics>(_props['physics']);
  bool get listViewShrinkWrap => extractBool(_props['shrink-wrap']) ?? false;
  EdgeInsetsGeometry? get listViewPadding =>
      extractEdgeInsets(_props['padding']);
  double? get listViewItemExtent => extractDouble(_props['item-extent']);
  Widget? get listViewPrototypeItem => extractChild(_props['prototype-item']);
  bool get listViewAddAutomaticKeepAlives =>
      extractBool(_props['add-automatic-keep-alives']) ?? true;
  bool get listViewAddRepaintBoundaries =>
      extractBool(_props['add-repaint-boundaries']) ?? true;
  bool get listViewAddSemanticIndexes =>
      extractBool(_props['add-semantic-indexes']) ?? true;
  double? get listViewCacheExtent => extractDouble(_props['cache-extent']);
  List<Widget>? get listViewChildren => extractChildren(_props['children']);
  int? get listViewSemanticChildCount =>
      extractInt(_props['semantic-child-count']);
  Clip get listViewClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.hardEdge;

  // GridView properties
  Axis get gridViewScrollDirection =>
      extractAxis(_props['scroll-direction']) ?? Axis.vertical;
  bool get gridViewReverse => extractBool(_props['reverse']) ?? false;
  ScrollController? get gridViewController =>
      extractNativeValue<ScrollController>(_props['controller']);
  bool get gridViewPrimary => extractBool(_props['primary']) ?? false;
  ScrollPhysics? get gridViewPhysics =>
      extractNativeValue<ScrollPhysics>(_props['physics']);
  bool get gridViewShrinkWrap => extractBool(_props['shrink-wrap']) ?? false;
  EdgeInsetsGeometry? get gridViewPadding =>
      extractEdgeInsets(_props['padding']);
  SliverGridDelegate? get gridViewGridDelegate =>
      extractNativeValue<SliverGridDelegate>(_props['grid-delegate']);
  bool get gridViewAddAutomaticKeepAlives =>
      extractBool(_props['add-automatic-keep-alives']) ?? true;
  bool get gridViewAddRepaintBoundaries =>
      extractBool(_props['add-repaint-boundaries']) ?? true;
  bool get gridViewAddSemanticIndexes =>
      extractBool(_props['add-semantic-indexes']) ?? true;
  double? get gridViewCacheExtent => extractDouble(_props['cache-extent']);
  List<Widget>? get gridViewChildren => extractChildren(_props['children']);
  int? get gridViewSemanticChildCount =>
      extractInt(_props['semantic-child-count']);
  Clip get gridViewClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.hardEdge;

  // SingleChildScrollView properties
  Axis get singleChildScrollViewScrollDirection =>
      extractAxis(_props['scroll-direction']) ?? Axis.vertical;
  bool get singleChildScrollViewReverse =>
      extractBool(_props['reverse']) ?? false;
  EdgeInsetsGeometry? get singleChildScrollViewPadding =>
      extractEdgeInsets(_props['padding']);
  bool get singleChildScrollViewPrimary =>
      extractBool(_props['primary']) ?? false;
  ScrollPhysics? get singleChildScrollViewPhysics =>
      extractNativeValue<ScrollPhysics>(_props['physics']);
  ScrollController? get singleChildScrollViewController =>
      extractNativeValue<ScrollController>(_props['controller']);
  DragStartBehavior get singleChildScrollViewDragStartBehavior =>
      extractDragStartBehavior(_props['drag-start-behavior']) ??
      DragStartBehavior.start;
  Clip get singleChildScrollViewClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.hardEdge;
  String? get singleChildScrollViewRestorationId =>
      extractString(_props['restoration-id']);
  ScrollViewKeyboardDismissBehavior
  get singleChildScrollViewKeyboardDismissBehavior =>
      extractNativeValue<ScrollViewKeyboardDismissBehavior>(
        _props['keyboard-dismiss-behavior'],
      ) ??
      ScrollViewKeyboardDismissBehavior.manual;
  Widget? get singleChildScrollViewChild => extractChild(_props['child']);

  // CustomScrollView properties
  Axis get customScrollViewScrollDirection =>
      extractAxis(_props['scroll-direction']) ?? Axis.vertical;
  bool get customScrollViewReverse => extractBool(_props['reverse']) ?? false;
  ScrollController? get customScrollViewController =>
      extractNativeValue<ScrollController>(_props['controller']);
  bool get customScrollViewPrimary => extractBool(_props['primary']) ?? false;
  ScrollPhysics? get customScrollViewPhysics =>
      extractNativeValue<ScrollPhysics>(_props['physics']);
  bool get customScrollViewShrinkWrap =>
      extractBool(_props['shrink-wrap']) ?? false;
  Key? get customScrollViewCenter => extractNativeValue<Key>(_props['center']);
  double get customScrollViewAnchor => extractDouble(_props['anchor']) ?? 0.0;
  double? get customScrollViewCacheExtent =>
      extractDouble(_props['cache-extent']);
  List<Widget>? get customScrollViewSlivers =>
      extractChildren(_props['slivers']);
  int? get customScrollViewSemanticChildCount =>
      extractInt(_props['semantic-child-count']);
  DragStartBehavior get customScrollViewDragStartBehavior =>
      extractDragStartBehavior(_props['drag-start-behavior']) ??
      DragStartBehavior.start;
  ScrollViewKeyboardDismissBehavior
  get customScrollViewKeyboardDismissBehavior =>
      extractNativeValue<ScrollViewKeyboardDismissBehavior>(
        _props['keyboard-dismiss-behavior'],
      ) ??
      ScrollViewKeyboardDismissBehavior.manual;
  String? get customScrollViewRestorationId =>
      extractString(_props['restoration-id']);
  Clip get customScrollViewClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.hardEdge;

  // SliverList properties
  SliverChildDelegate? get sliverListDelegate =>
      extractNativeValue<SliverChildDelegate>(_props['delegate']);

  // SliverGrid properties
  SliverChildDelegate? get sliverGridDelegate =>
      extractNativeValue<SliverChildDelegate>(_props['delegate']);
  SliverGridDelegate? get sliverGridGridDelegate =>
      extractNativeValue<SliverGridDelegate>(_props['grid-delegate']);

  // NavigationRail properties
  Color? get navigationRailBackgroundColor =>
      extractColor(_props['background-color']);
  bool get navigationRailExtended => extractBool(_props['extended']) ?? false;
  Widget? get navigationRailLeading => extractChild(_props['leading']);
  Widget? get navigationRailTrailing => extractChild(_props['trailing']);
  List<NavigationRailDestination>? get navigationRailDestinations =>
      extractNativeValue<List<NavigationRailDestination>>(
        _props['destinations'],
      );
  int? get navigationRailSelectedIndex => extractInt(_props['selected-index']);
  ValueChanged<int>? get navigationRailOnDestinationSelected =>
      extractNativeValue<ValueChanged<int>>(_props['on-destination-selected']);
  double get navigationRailElevation =>
      extractDouble(_props['elevation']) ?? 0.0;
  double get navigationRailGroupAlignment =>
      extractDouble(_props['group-alignment']) ?? -1.0;
  NavigationRailLabelType? get navigationRailLabelType =>
      extractNativeValue<NavigationRailLabelType>(_props['label-type']);
  TextStyle? get navigationRailUnselectedLabelTextStyle =>
      extractNativeValue<TextStyle>(_props['unselected-label-text-style']);
  TextStyle? get navigationRailSelectedLabelTextStyle =>
      extractNativeValue<TextStyle>(_props['selected-label-text-style']);
  IconThemeData? get navigationRailUnselectedIconTheme =>
      extractNativeValue<IconThemeData>(_props['unselected-icon-theme']);
  IconThemeData? get navigationRailSelectedIconTheme =>
      extractNativeValue<IconThemeData>(_props['selected-icon-theme']);
  double get navigationRailMinWidth =>
      extractDouble(_props['min-width']) ?? 72.0;
  double get navigationRailMinExtendedWidth =>
      extractDouble(_props['min-extended-width']) ?? 256.0;
  bool get navigationRailUseIndicator =>
      extractBool(_props['use-indicator']) ?? true;
  Color? get navigationRailIndicatorColor =>
      extractColor(_props['indicator-color']);
  ShapeBorder? get navigationRailIndicatorShape =>
      extractNativeValue<ShapeBorder>(_props['indicator-shape']);
  bool get navigationRailLeadingAtTop =>
      extractBool(_props['leading-at-top']) ?? true;
  bool get navigationRailTrailingAtBottom =>
      extractBool(_props['trailing-at-bottom']) ?? false;
  bool get navigationRailScrollable =>
      extractBool(_props['scrollable']) ?? false;

  // CupertinoButton properties
  CupertinoButtonSize get cupertinoButtonSizeStyle =>
      extractNativeValue<CupertinoButtonSize>(_props['size-style']) ??
      CupertinoButtonSize.large;
  EdgeInsetsGeometry get cupertinoButtonPadding =>
      extractEdgeInsets(_props['padding']) ??
      const EdgeInsets.symmetric(vertical: 16.0, horizontal: 30.0);
  Color get cupertinoButtonDisabledColor =>
      extractColor(_props['disabled-color']) ??
      CupertinoColors.quaternarySystemFill;
  double get cupertinoButtonPressedOpacity =>
      extractDouble(_props['pressed-opacity']) ?? 0.4;
  BorderRadius get cupertinoButtonBorderRadius =>
      extractNativeValue<BorderRadius>(_props['border-radius']) ??
      const BorderRadius.all(Radius.circular(8.0));

  // CupertinoActivityIndicator properties
  bool get cupertinoActivityIndicatorAnimating =>
      extractBool(_props['animating']) ?? true;
  double get cupertinoActivityIndicatorRadius =>
      extractDouble(_props['radius']) ?? 10.0;

  // CupertinoNavigationBar properties
  Widget? get cupertinoNavigationBarLeading => extractChild(_props['leading']);
  bool get cupertinoNavigationBarAutomaticallyImplyLeading =>
      extractBool(_props['automatically-imply-leading']) ?? true;
  bool get cupertinoNavigationBarAutomaticallyImplyMiddle =>
      extractBool(_props['automatically-imply-middle']) ?? true;
  String? get cupertinoNavigationBarPreviousPageTitle =>
      extractString(_props['previous-page-title']);
  Widget get cupertinoNavigationBarMiddle => extractChild(_props['middle'])!;
  Widget? get cupertinoNavigationBarTrailing =>
      extractChild(_props['trailing']);
  Border get cupertinoNavigationBarBorder =>
      extractNativeValue<Border>(_props['border']) ??
      const Border(bottom: BorderSide(color: Color(0x4D000000), width: 0.0));
  Color? get cupertinoNavigationBarBackgroundColor =>
      extractColor(_props['background-color']);
  Brightness? get cupertinoNavigationBarBrightness =>
      extractNativeValue<Brightness>(_props['brightness']);
  EdgeInsetsDirectional get cupertinoNavigationBarPadding =>
      extractNativeValue<EdgeInsetsDirectional>(_props['padding']) ??
      EdgeInsetsDirectional.zero;
  bool get cupertinoNavigationBarTransitionBetweenRoutes =>
      extractBool(_props['transition-between-routes']) ?? true;
  Object get cupertinoNavigationBarHeroTag =>
      extractNativeValue<Object>(_props['hero-tag']) ?? _defaultHeroTag;

  // CupertinoPageScaffold properties
  Color? get cupertinoPageScaffoldBackgroundColor =>
      extractColor(_props['background-color']);
  bool get cupertinoPageScaffoldResizeToAvoidBottomInset =>
      extractBool(_props['resize-to-avoid-bottom-inset']) ?? true;

  // CupertinoTextField properties
  TextEditingController? get cupertinoTextFieldController =>
      extractNativeValue<TextEditingController>(_props['controller']);
  String? get cupertinoTextFieldPlaceholder =>
      extractString(_props['placeholder']);
  TextStyle get cupertinoTextFieldPlaceholderStyle =>
      extractNativeValue<TextStyle>(_props['placeholder-style']) ??
      const TextStyle(
        fontWeight: FontWeight.w400,
        color: CupertinoColors.placeholderText,
      );
  Widget? get cupertinoTextFieldPrefix => extractChild(_props['prefix']);
  OverlayVisibilityMode get cupertinoTextFieldPrefixMode =>
      extractNativeValue<OverlayVisibilityMode>(_props['prefix-mode']) ??
      OverlayVisibilityMode.always;
  Widget? get cupertinoTextFieldSuffix => extractChild(_props['suffix']);
  OverlayVisibilityMode get cupertinoTextFieldSuffixMode =>
      extractNativeValue<OverlayVisibilityMode>(_props['suffix-mode']) ??
      OverlayVisibilityMode.always;
  OverlayVisibilityMode get cupertinoTextFieldClearButtonMode =>
      extractNativeValue<OverlayVisibilityMode>(_props['clear-button-mode']) ??
      OverlayVisibilityMode.never;
  Color get cupertinoTextFieldDecorationBorderColor =>
      extractColor(_props['decoration-border-color']) ??
      CupertinoColors.inactiveGray;
  BoxDecoration? get cupertinoTextFieldDecoration =>
      extractNativeValue<BoxDecoration>(_props['decoration']) ??
      const BoxDecoration(
        border: Border.fromBorderSide(
          BorderSide(width: 0.0, color: CupertinoColors.inactiveGray),
        ),
        borderRadius: BorderRadius.all(Radius.circular(5.0)),
      );
  EdgeInsetsGeometry get cupertinoTextFieldPadding =>
      extractEdgeInsets(_props['padding']) ?? const EdgeInsets.all(6.0);

  // CupertinoSwitch properties
  bool get cupertinoSwitchValue => extractBool(_props['value']) ?? false;

  // CupertinoDatePicker properties
  CupertinoDatePickerMode get cupertinoDatePickerMode =>
      extractNativeValue<CupertinoDatePickerMode>(_props['mode']) ??
      CupertinoDatePickerMode.dateAndTime;
  ValueChanged<DateTime> get cupertinoDatePickerOnDateTimeChanged =>
      extractNativeValue<ValueChanged<DateTime>>(
        _props['on-date-time-changed'],
      )!;
  DateTime? get cupertinoDatePickerInitialDateTime =>
      extractNativeValue<DateTime>(_props['initial-date-time']);
  DateTime? get cupertinoDatePickerMinimumDate =>
      extractNativeValue<DateTime>(_props['minimum-date']);
  DateTime? get cupertinoDatePickerMaximumDate =>
      extractNativeValue<DateTime>(_props['maximum-date']);
  int get cupertinoDatePickerMinimumYear =>
      extractInt(_props['minimum-year']) ?? 1;
  int? get cupertinoDatePickerMaximumYear => extractInt(_props['maximum-year']);
  int get cupertinoDatePickerMinuteInterval =>
      extractInt(_props['minute-interval']) ?? 1;
  bool get cupertinoDatePickerUse24hFormat =>
      extractBool(_props['use-24h-format']) ?? false;
  DatePickerDateOrder? get cupertinoDatePickerDateOrder =>
      extractNativeValue<DatePickerDateOrder>(_props['date-order']);
  Color? get cupertinoDatePickerBackgroundColor =>
      extractColor(_props['background-color']);
  bool get cupertinoDatePickerShowDayOfWeek =>
      extractBool(_props['show-day-of-week']) ?? false;
  double get cupertinoDatePickerItemExtent =>
      extractDouble(_props['item-extent']) ?? 32.0;

  // CupertinoPicker properties
  double get cupertinoPickerDiameterRatio =>
      extractDouble(_props['diameter-ratio']) ?? 1.07;
  Color? get cupertinoPickerBackgroundColor =>
      extractColor(_props['background-color']);
  double get cupertinoPickerOffAxisFraction =>
      extractDouble(_props['off-axis-fraction']) ?? 0.0;
  bool get cupertinoPickerUseMagnifier =>
      extractBool(_props['use-magnifier']) ?? false;
  double get cupertinoPickerMagnification =>
      extractDouble(_props['magnification']) ?? 1.0;
  FixedExtentScrollController? get cupertinoPickerScrollController =>
      extractNativeValue<FixedExtentScrollController>(
        _props['scroll-controller'],
      );
  double get cupertinoPickerSqueeze => extractDouble(_props['squeeze']) ?? 1.45;
  double get cupertinoPickerItemExtent =>
      extractDouble(_props['item-extent']) ?? 32.0;
  ValueChanged<int>? get cupertinoPickerOnSelectedItemChanged =>
      extractNativeValue<ValueChanged<int>>(_props['on-selected-item-changed']);
  CupertinoPickerDefaultSelectionOverlay? get cupertinoPickerSelectionOverlay =>
      extractNativeValue<CupertinoPickerDefaultSelectionOverlay>(
        _props['selection-overlay'],
      );

  // CupertinoScrollable properties
  ScrollController? get cupertinoScrollableController =>
      extractNativeValue<ScrollController>(_props['controller']);
  double get cupertinoScrollableThickness =>
      extractDouble(_props['thickness']) ?? 3.0;
  double get cupertinoScrollableThicknessWhileDragging =>
      extractDouble(_props['thickness-while-dragging']) ?? 8.0;
  Radius get cupertinoScrollableRadius =>
      extractNativeValue<Radius>(_props['radius']) ??
      const Radius.circular(1.5);
  Radius get cupertinoScrollableRadiusWhileDragging =>
      extractNativeValue<Radius>(_props['radius-while-dragging']) ??
      const Radius.circular(4.0);
  ScrollNotificationPredicate? get cupertinoScrollableNotificationPredicate =>
      extractNativeValue<ScrollNotificationPredicate>(
        _props['notification-predicate'],
      );

  // CupertinoSearchTextField properties
  SearchController? get cupertinoSearchTextFieldController =>
      extractNativeValue<SearchController>(_props['controller']);
  ValueChanged<String>? get cupertinoSearchTextFieldOnChanged =>
      extractNativeValue<ValueChanged<String>>(_props['on-changed']);
  ValueChanged<String>? get cupertinoSearchTextFieldOnSubmitted =>
      extractNativeValue<ValueChanged<String>>(_props['on-submitted']);
  TextStyle? get cupertinoSearchTextFieldStyle =>
      extractNativeValue<TextStyle>(_props['style']);
  String? get cupertinoSearchTextFieldHintText =>
      extractString(_props['hint-text']);
  Widget? get cupertinoSearchTextFieldPrefixIcon =>
      extractChild(_props['prefix-icon']);
  Widget? get cupertinoSearchTextFieldSuffixIcon =>
      extractChild(_props['suffix-icon']);
  OverlayVisibilityMode get cupertinoSearchTextFieldPrefixMode =>
      extractNativeValue<OverlayVisibilityMode>(_props['prefix-mode']) ??
      OverlayVisibilityMode.always;
  OverlayVisibilityMode get cupertinoSearchTextFieldSuffixMode =>
      extractNativeValue<OverlayVisibilityMode>(_props['suffix-mode']) ??
      OverlayVisibilityMode.always;
  VoidCallback? get cupertinoSearchTextFieldOnSuffixTap =>
      extractNativeValue<VoidCallback>(_props['on-suffix-tap']);
  BoxDecoration? get cupertinoSearchTextFieldDecoration =>
      extractNativeValue<BoxDecoration>(_props['decoration']);
  Color? get cupertinoSearchTextFieldBackgroundColor =>
      extractColor(_props['background-color']);
  BorderRadius? get cupertinoSearchTextFieldBorderRadius =>
      extractNativeValue<BorderRadius>(_props['border-radius']);
  EdgeInsetsGeometry get cupertinoSearchTextFieldPadding =>
      extractEdgeInsets(_props['padding']) ??
      const EdgeInsetsDirectional.fromSTEB(5.5, 8, 5.5, 8);
  Color? get cupertinoSearchTextFieldItemColor =>
      extractColor(_props['item-color']);
  double? get cupertinoSearchTextFieldItemSize =>
      extractDouble(_props['item-size']);

  // CupertinoSegmentedControl properties
  Map<Object, Widget> get cupertinoSegmentedControlChildren =>
      extractNativeValue<Map<Object, Widget>>(_props['children']) ?? {};
  ValueChanged<Object>? get cupertinoSegmentedControlOnValueChanged =>
      extractNativeValue<ValueChanged<Object>>(_props['on-value-changed']);
  Object? get cupertinoSegmentedControlGroupValue =>
      extractNativeValue<Object>(_props['group-value']);
  Color get cupertinoSegmentedControlUnselectedColor =>
      extractColor(_props['unselected-color']) ??
      CupertinoColors.tertiarySystemFill;
  Color get cupertinoSegmentedControlSelectedColor =>
      extractColor(_props['selected-color']) ?? CupertinoColors.systemBlue;
  Color get cupertinoSegmentedControlBorderColor =>
      extractColor(_props['border-color']) ?? CupertinoColors.systemGrey4;
  Color? get cupertinoSegmentedControlPressedColor =>
      extractColor(_props['pressed-color']);
  EdgeInsetsGeometry get cupertinoSegmentedControlPadding =>
      extractEdgeInsets(_props['padding']) ??
      const EdgeInsets.symmetric(vertical: 2, horizontal: 3);

  // CupertinoSlider properties
  double get cupertinoSliderValue => extractDouble(_props['value']) ?? 0.0;
  double get cupertinoSliderMin => extractDouble(_props['min']) ?? 0.0;
  double get cupertinoSliderMax => extractDouble(_props['max']) ?? 1.0;
  int? get cupertinoSliderDivisions => extractInt(_props['divisions']);
  Color? get cupertinoSliderActiveColor => extractColor(_props['active-color']);
  Color get cupertinoSliderThumbColor =>
      extractColor(_props['thumb-color']) ?? CupertinoColors.white;
  // CupertinoSlidingSegmentedControl properties
  Map<dynamic, Widget> get cupertinoSlidingSegmentedControlChildren =>
      extractNativeValue<Map<dynamic, Widget>>(_props['children']) ?? {};
  ValueChanged<dynamic>? get cupertinoSlidingSegmentedControlOnValueChanged =>
      extractNativeValue<ValueChanged<dynamic>>(_props['on-value-changed']);
  dynamic get cupertinoSlidingSegmentedControlGroupValue =>
      extractNativeValue<dynamic>(_props['group-value']);
  Color get cupertinoSlidingSegmentedControlThumbColor =>
      extractColor(_props['thumb-color']) ?? const Color(0xFFFFFFFF);
  Color? get cupertinoSlidingSegmentedControlBackgroundColor =>
      extractColor(_props['background-color']);
  EdgeInsetsGeometry get cupertinoSlidingSegmentedControlPadding =>
      extractEdgeInsets(_props['padding']) ??
      const EdgeInsets.symmetric(vertical: 2, horizontal: 3);

  // CupertinoTimerPicker properties
  CupertinoTimerPickerMode get cupertinoTimerPickerMode =>
      extractNativeValue<CupertinoTimerPickerMode>(_props['mode']) ??
      CupertinoTimerPickerMode.hms;
  Duration get cupertinoTimerPickerInitialTimerDuration =>
      extractNativeValue<Duration>(_props['initial-timer-duration']) ??
      Duration.zero;
  int get cupertinoTimerPickerMinuteInterval =>
      extractInt(_props['minute-interval']) ?? 1;
  int get cupertinoTimerPickerSecondInterval =>
      extractInt(_props['second-interval']) ?? 1;
  AlignmentGeometry get cupertinoTimerPickerAlignment =>
      extractNativeValue<AlignmentGeometry>(_props['alignment']) ??
      Alignment.center;
  Color? get cupertinoTimerPickerBackgroundColor =>
      extractColor(_props['background-color']);
  double get cupertinoTimerPickerItemExtent =>
      extractDouble(_props['item-extent']) ?? 32.0;
  ValueChanged<Duration> get cupertinoTimerPickerOnTimerDurationChanged =>
      extractNativeValue<ValueChanged<Duration>>(
        _props['on-timer-duration-changed'],
      )!;

  // CupertinoTabBar properties
  List<BottomNavigationBarItem> get cupertinoTabBarItems =>
      extractNativeValue<List<BottomNavigationBarItem>>(_props['items']) ?? [];
  ValueChanged<int>? get cupertinoTabBarOnTap =>
      extractNativeValue<ValueChanged<int>>(_props['on-press']);
  int get cupertinoTabBarCurrentIndex =>
      extractInt(_props['current-index']) ?? 0;
  Color? get cupertinoTabBarBackgroundColor =>
      extractColor(_props['background-color']);
  Color get cupertinoTabBarActiveColor =>
      extractColor(_props['active-color']) ?? CupertinoColors.activeBlue;
  Color get cupertinoTabBarInactiveColor =>
      extractColor(_props['inactive-color']) ?? CupertinoColors.inactiveGray;
  double get cupertinoTabBarIconSize =>
      extractDouble(_props['icon-size']) ?? 30.0;
  Border get cupertinoTabBarBorder =>
      extractNativeValue<Border>(_props['border']) ??
      const Border(top: BorderSide(color: Color(0x4D000000), width: 0.0));

  // CupertinoTabScaffold properties
  CupertinoTabBar get cupertinoTabScaffoldTabBar =>
      extractNativeValue<CupertinoTabBar>(_props['tab-bar'])!;
  IndexedWidgetBuilder get cupertinoTabScaffoldTabBuilder =>
      extractNativeValue<IndexedWidgetBuilder>(_props['tab-builder'])!;
  CupertinoTabController? get cupertinoTabScaffoldController =>
      extractNativeValue<CupertinoTabController>(_props['controller']);
  Color? get cupertinoTabScaffoldBackgroundColor =>
      extractColor(_props['background-color']);
  bool get cupertinoTabScaffoldResizeToAvoidBottomInset =>
      extractBool(_props['resize-to-avoid-bottom-inset']) ?? true;
  String? get cupertinoTabScaffoldRestorationId =>
      extractString(_props['restoration-id']);

  // CupertinoContextMenu properties
  List<Widget> get cupertinoContextMenuActions =>
      extractChildren(_props['actions']) ?? [];
  Widget? get cupertinoContextMenuPreviewBuilder =>
      extractChild(_props['preview-builder']);

  // CupertinoActionSheet properties
  Widget? get cupertinoActionSheetTitle =>
      extractChild(_props['cupertino-action-sheet-title']);
  Widget? get cupertinoActionSheetMessage =>
      extractChild(_props['cupertino-action-sheet-message']);
  List<Widget>? get cupertinoActionSheetActions =>
      extractChildren(_props['cupertino-action-sheet-actions']);
  ScrollController? get cupertinoActionSheetMessageScrollController =>
      extractNativeValue<ScrollController>(
        _props['cupertino-action-sheet-message-scroll-controller'],
      );
  ScrollController? get cupertinoActionSheetActionScrollController =>
      extractNativeValue<ScrollController>(
        _props['cupertino-action-sheet-action-scroll-controller'],
      );
  Widget? get cupertinoActionSheetCancelButton =>
      extractChild(_props['cupertino-action-sheet-cancel-button']);

  // CupertinoAlertDialog properties
  Widget? get cupertinoAlertDialogTitle =>
      extractChild(_props['cupertino-alert-dialog-title']);
  Widget? get cupertinoAlertDialogContent =>
      extractChild(_props['cupertino-alert-dialog-content']);
  List<Widget> get cupertinoAlertDialogActions =>
      extractChildren(_props['cupertino-alert-dialog-actions']) ?? [];
  ScrollController? get cupertinoAlertDialogScrollController =>
      extractNativeValue<ScrollController>(
        _props['cupertino-alert-dialog-scroll-controller'],
      );
  ScrollController? get cupertinoAlertDialogActionScrollController =>
      extractNativeValue<ScrollController>(
        _props['cupertino-alert-dialog-action-scroll-controller'],
      );
  Duration get cupertinoAlertDialogInsetAnimationDuration =>
      extractNativeValue<Duration>(
        _props['cupertino-alert-dialog-inset-animation-duration'],
      ) ??
      const Duration(milliseconds: 100);
  Curve get cupertinoAlertDialogInsetAnimationCurve =>
      extractNativeValue<Curve>(
        _props['cupertino-alert-dialog-inset-animation-curve'],
      ) ??
      Curves.decelerate;

  // Generic scroll properties
  ScrollController? get scrollController =>
      extractNativeValue<ScrollController>(_props['scroll-controller']);
  ScrollNotificationPredicate get scrollNotificationPredicate =>
      extractNativeValue<ScrollNotificationPredicate>(
        _props['notification-predicate'],
      ) ??
      defaultScrollNotificationPredicate;

  // Generic alert dialog properties
  Widget? get alertDialogTitle => extractChild(_props['title']);
  Widget? get alertDialogContent => extractChild(_props['content']);

  // CupertinoScrollbar properties
  bool get cupertinoScrollbarThumbVisibility =>
      extractBool(_props['thumb-visibility']) ?? true;
  double get cupertinoScrollbarThickness =>
      extractDouble(_props['thickness']) ?? 3.0;
  double get cupertinoScrollbarThicknessWhileDragging =>
      extractDouble(_props['thickness-while-dragging']) ?? 8.0;
  Radius get cupertinoScrollbarRadius =>
      extractNativeValue<Radius>(_props['radius']) ??
      const Radius.circular(1.5);
  Radius get cupertinoScrollbarRadiusWhileDragging =>
      extractNativeValue<Radius>(_props['radius-while-dragging']) ??
      const Radius.circular(4.0);

  // CupertinoSlider properties
  ValueChanged<double>? get cupertinoSliderOnChanged =>
      extractNativeValue<ValueChanged<double>>(_props['on-changed']);
  ValueChanged<double>? get cupertinoSliderOnChangeStart =>
      extractNativeValue<ValueChanged<double>>(_props['on-change-start']);
  ValueChanged<double>? get cupertinoSliderOnChangeEnd =>
      extractNativeValue<ValueChanged<double>>(_props['on-change-end']);

  // CupertinoPageScaffold properties
  Widget? get cupertinoPageScaffoldChild => extractChild(_props['child']);
  ObstructingPreferredSizeWidget? get cupertinoPageScaffoldNavigationBar =>
      extractNativeValue<ObstructingPreferredSizeWidget>(
        _props['navigation-bar'],
      );
  // bool? get cupertinoPageScaffoldResizeToAvoidBottomInset =>
  //     extractBool(_props['resize-to-avoid-bottom-inset']);
  // Widget? get cupertinoPageScaffoldChild => extractChild(_props['child']);

  // CupertinoCheckbox properties
  bool? get cupertinoCheckboxValue => extractBool(_props['value']);
  bool get cupertinoCheckboxTristate =>
      extractBool(_props['tristate']) ?? false;
  ValueChanged<bool?>? get cupertinoCheckboxOnChanged =>
      extractNativeValue<ValueChanged<bool?>>(_props['on-changed']);
  Color? get cupertinoCheckboxActiveColor =>
      extractColor(_props['active-color']);
  Color? get cupertinoCheckboxInactiveColor =>
      extractColor(_props['inactive-color']);
  Color? get cupertinoCheckboxCheckColor => extractColor(_props['check-color']);
  Color? get cupertinoCheckboxFocusColor => extractColor(_props['focus-color']);
}
