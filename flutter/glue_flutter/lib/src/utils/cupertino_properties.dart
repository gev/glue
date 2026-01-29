import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart' show SearchController;
import 'package:glue_flutter/src/utils/core_properties.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Default hero tag for navigation bars
const _defaultHeroTag = '<default-hero-tag>';

/// Properties wrapper with lazy getters
/// Provides clean API for accessing widget properties without pre-computing everything
class CupertinoProperties extends CoreProperties {
  CupertinoProperties(super.props);
  CupertinoProperties.empty() : super.empty();

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
