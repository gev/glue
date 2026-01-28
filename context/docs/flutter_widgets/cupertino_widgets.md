
# Cupertino Widgets

This section provides constructor signatures and parameter details for Cupertino (iOS-style) widgets.

## Widget Constructor Reference

### CupertinoActionSheet
Short description: An iOS-style action sheet for presenting a set of actions.
Documentation: [CupertinoActionSheet](https://api.flutter.dev/flutter/cupertino/CupertinoActionSheet-class.html)

**Constructor:**
```dart
CupertinoActionSheet({
  Key? key,
  Widget? title,
  Widget? message,
  List<Widget>? actions,
  ScrollController? messageScrollController,
  ScrollController? actionScrollController,
  Widget? cancelButton,
})
```

### CupertinoActivityIndicator
Short description: An iOS-style activity indicator (spinner).
Documentation: [CupertinoActivityIndicator](https://api.flutter.dev/flutter/cupertino/CupertinoActivityIndicator-class.html)

**Constructor:**
```dart
CupertinoActivityIndicator({
  Key? key,
  Color? color,
  bool animating = true,
  double radius = 10.0,
})
```

### CupertinoAlertDialog
Short description: An iOS-style alert dialog.
Documentation: [CupertinoAlertDialog](https://api.flutter.dev/flutter/cupertino/CupertinoAlertDialog-class.html)

**Constructor:**
```dart
CupertinoAlertDialog({
  Key? key,
  Widget? title,
  Widget? content,
  List<Widget> actions = const <Widget>[],
  ScrollController? scrollController,
  ScrollController? actionScrollController,
  Duration insetAnimationDuration = const Duration(milliseconds: 100),
  Curve insetAnimationCurve = Curves.decelerate,
})
```

### CupertinoApp
Short description: An application that uses Cupertino design.
Documentation: [CupertinoApp](https://api.flutter.dev/flutter/cupertino/CupertinoApp-class.html)

**Constructor:**
```dart
CupertinoApp({
  Key? key,
  GlobalKey<NavigatorState>? navigatorKey,
  Widget? home,
  CupertinoThemeData? theme,
  Map<String, WidgetBuilder> routes = const <String, WidgetBuilder>{},
  String? initialRoute,
  RouteFactory? onGenerateRoute,
  InitialRouteListFactory? onGenerateInitialRoutes,
  RouteFactory? onUnknownRoute,
  List<NavigatorObserver> navigatorObservers = const <NavigatorObserver>[],
  TransitionBuilder? builder,
  String title = '',
  GenerateAppTitle? onGenerateTitle,
  Color? color,
  Locale? locale,
  Iterable<LocalizationsDelegate<dynamic>>? localizationsDelegates,
  LocaleListResolutionCallback? localeListResolutionCallback,
  LocaleResolutionCallback? localeResolutionCallback,
  Iterable<Locale> supportedLocales = const <Locale>[Locale('en', 'US')],
  bool showPerformanceOverlay = false,
  bool checkerboardRasterCacheImages = false,
  bool checkerboardOffscreenLayers = false,
  bool showSemanticsDebugger = false,
  bool debugShowCheckedModeBanner = true,
  Map<LogicalKeySet, Intent>? shortcuts,
  Map<Type, Action<Intent>>? actions,
  String? restorationScopeId,
  ScrollBehavior? scrollBehavior,
})
```

### CupertinoButton
Short description: An iOS-style button.
Documentation: [CupertinoButton](https://api.flutter.dev/flutter/cupertino/CupertinoButton-class.html)

**Constructor:**
```dart
CupertinoButton({
  Key? key,
  required Widget child,
  EdgeInsetsGeometry? padding,
  Color? color,
  Color disabledColor = CupertinoColors.quaternarySystemFill,
  double? minSize = 44.0,
  double? pressedOpacity = 0.4,
  BorderRadius? borderRadius = const BorderRadius.all(Radius.circular(8.0)),
  AlignmentGeometry alignment = Alignment.center,
  required VoidCallback? onPressed,
})
```

### CupertinoCheckbox
Short description: An iOS-style checkbox.
Documentation: [CupertinoCheckbox](https://api.flutter.dev/flutter/cupertino/CupertinoCheckbox-class.html)

**Constructor:**
```dart
CupertinoCheckbox({
  Key? key,
  required bool? value,
  bool tristate = false,
  required ValueChanged<bool?>? onChanged,
  Color? activeColor,
  Color? inactiveColor,
  Color? checkColor,
  Color? focusColor,
  FocusNode? focusNode,
  bool autofocus = false,
  OutlinedBorder? shape,
  BorderSide? side,
})
```

### CupertinoContextMenu
Short description: An iOS-style context menu.
Documentation: [CupertinoContextMenu](https://api.flutter.dev/flutter/cupertino/CupertinoContextMenu-class.html)

**Constructor:**
```dart
CupertinoContextMenu({
  Key? key,
  required List<Widget> actions,
  required Widget child,
  Widget? previewBuilder,
})
```

### CupertinoDatePicker
Short description: An iOS-style date picker.
Documentation: [CupertinoDatePicker](https://api.flutter.dev/flutter/cupertino/CupertinoDatePicker-class.html)

**Constructor:**
```dart
CupertinoDatePicker({
  Key? key,
  CupertinoDatePickerMode mode = CupertinoDatePickerMode.dateAndTime,
  required ValueChanged<DateTime> onDateTimeChanged,
  DateTime? initialDateTime,
  DateTime? minimumDate,
  DateTime? maximumDate,
  int minimumYear = 1,
  int? maximumYear,
  int minuteInterval = 1,
  bool use24hFormat = false,
  DatePickerDateOrder? dateOrder,
  Color? backgroundColor,
  bool showDayOfWeek = false,
  double itemExtent = 32.0,
})
```

### CupertinoNavigationBar
Short description: An iOS-style navigation bar.
Documentation: [CupertinoNavigationBar](https://api.flutter.dev/flutter/cupertino/CupertinoNavigationBar-class.html)

**Constructor:**
```dart
CupertinoNavigationBar({
  Key? key,
  Widget? leading,
  bool automaticallyImplyLeading = true,
  bool automaticallyImplyMiddle = true,
  String? previousPageTitle,
  Widget? middle,
  Widget? trailing,
  Border? border = const Border(bottom: BorderSide(color: Color(0x4D000000), width: 0.0)),
  Color? backgroundColor,
  Brightness? brightness,
  EdgeInsetsDirectional? padding,
  bool transitionBetweenRoutes = true,
  Object heroTag = _defaultHeroTag,
})
```

### CupertinoPageScaffold
Short description: Basic layout structure for a Cupertino page.
Documentation: [CupertinoPageScaffold](https://api.flutter.dev/flutter/cupertino/CupertinoPageScaffold-class.html)

**Constructor:**
```dart
CupertinoPageScaffold({
  Key? key,
  ObstructingPreferredSizeWidget? navigationBar,
  Color? backgroundColor,
  bool resizeToAvoidBottomInset = true,
  required Widget child,
})
```

### CupertinoPicker
Short description: An iOS-style picker control.
Documentation: [CupertinoPicker](https://api.flutter.dev/flutter/cupertino/CupertinoPicker-class.html)

**Constructor:**
```dart
CupertinoPicker({
  Key? key,
  double diameterRatio = 1.07,
  Color? backgroundColor,
  double offAxisFraction = 0.0,
  bool useMagnifier = false,
  double magnification = 1.0,
  FixedExtentScrollController? scrollController,
  double squeeze = 1.45,
  required double itemExtent,
  required ValueChanged<int>? onSelectedItemChanged,
  required List<Widget> children,
  Widget? selectionOverlay = const CupertinoPickerDefaultSelectionOverlay(),
})
```

### CupertinoScrollbar
Short description: An iOS-style scrollbar.
Documentation: [CupertinoScrollbar](https://api.flutter.dev/flutter/cupertino/CupertinoScrollbar-class.html)

**Constructor:**
```dart
CupertinoScrollbar({
  Key? key,
  required Widget child,
  ScrollController? controller,
  bool? thumbVisibility,
  double thickness = 3.0,
  double thicknessWhileDragging = 8.0,
  Radius radius = const Radius.circular(1.5),
  Radius radiusWhileDragging = const Radius.circular(4.0),
  ScrollNotificationPredicate? notificationPredicate,
})
```

### CupertinoSearchTextField
Short description: An iOS-style search text field.
Documentation: [CupertinoSearchTextField](https://api.flutter.dev/flutter/cupertino/CupertinoSearchTextField-class.html)

**Constructor:**
```dart
CupertinoSearchTextField({
  Key? key,
  TextEditingController? controller,
  ValueChanged<String>? onChanged,
  ValueChanged<String>? onSubmitted,
  TextStyle? style,
  String? placeholder,
  TextStyle? placeholderStyle,
  BoxDecoration? decoration,
  Color? backgroundColor,
  BorderRadius? borderRadius,
  EdgeInsetsGeometry padding = const EdgeInsetsDirectional.fromSTEB(5.5, 8, 5.5, 8),
  Color? itemColor,
  double? itemSize,
  Widget? prefixIcon,
  OverlayVisibilityMode prefixMode = OverlayVisibilityMode.always,
  Widget? suffixIcon,
  OverlayVisibilityMode suffixMode = OverlayVisibilityMode.always,
  VoidCallback? onSuffixTap,
  bool? enabled,
  bool autocorrect = true,
  FocusNode? focusNode,
  bool autofocus = false,
})
```

### CupertinoSegmentedControl
Short description: An iOS-style segmented control.
Documentation: [CupertinoSegmentedControl](https://api.flutter.dev/flutter/cupertino/CupertinoSegmentedControl-class.html)

**Constructor:**
```dart
CupertinoSegmentedControl({
  Key? key,
  required Map<T, Widget> children,
  required ValueChanged<T>? onValueChanged,
  T? groupValue,
  Color unselectedColor = CupertinoColors.tertiarySystemFill,
  Color selectedColor = CupertinoColors.systemBlue,
  Color borderColor = CupertinoColors.systemGrey4,
  Color? pressedColor,
  EdgeInsetsGeometry padding = const EdgeInsets.symmetric(vertical: 2, horizontal: 3),
})
```

### CupertinoSlider
Short description: An iOS-style slider.
Documentation: [CupertinoSlider](https://api.flutter.dev/flutter/cupertino/CupertinoSlider-class.html)

**Constructor:**
```dart
CupertinoSlider({
  Key? key,
  required double value,
  required ValueChanged<double>? onChanged,
  ValueChanged<double>? onChangeStart,
  ValueChanged<double>? onChangeEnd,
  double min = 0.0,
  double max = 1.0,
  int? divisions,
  Color? activeColor,
  Color thumbColor = CupertinoColors.white,
})
```

### CupertinoSlidingSegmentedControl
Short description: An iOS 13 style sliding segmented control.
Documentation: [CupertinoSlidingSegmentedControl](https://api.flutter.dev/flutter/cupertino/CupertinoSlidingSegmentedControl-class.html)

**Constructor:**
```dart
CupertinoSlidingSegmentedControl({
  Key? key,
  required Map<T, Widget> children,
  required ValueChanged<T?>? onValueChanged,
  T? groupValue,
  Color thumbColor = const Color(0xFFFFFFFF),
  Color? backgroundColor,
  EdgeInsetsGeometry padding = const EdgeInsets.symmetric(vertical: 2, horizontal: 3),
})
```

### CupertinoSwitch
Short description: An iOS-style switch.
Documentation: [CupertinoSwitch](https://api.flutter.dev/flutter/cupertino/CupertinoSwitch-class.html)

**Constructor:**
```dart
CupertinoSwitch({
  Key? key,
  required bool value,
  required ValueChanged<bool>? onChanged,
  Color? activeColor,
  Color? trackColor,
  Color? thumbColor,
  DragStartBehavior dragStartBehavior = DragStartBehavior.start,
  FocusNode? focusNode,
  bool autofocus = false,
})
```

### CupertinoTabBar
Short description: An iOS-style bottom tab bar.
Documentation: [CupertinoTabBar](https://api.flutter.dev/flutter/cupertino/CupertinoTabBar-class.html)

**Constructor:**
```dart
CupertinoTabBar({
  Key? key,
  required List<BottomNavigationBarItem> items,
  ValueChanged<int>? onTap,
  int currentIndex = 0,
  Color? backgroundColor,
  Color? activeColor,
  Color inactiveColor = CupertinoColors.inactiveGray,
  double iconSize = 30.0,
  Border? border = const Border(top: BorderSide(color: Color(0x4D000000), width: 0.0)),
})
```

### CupertinoTabScaffold
Short description: A scaffold for apps with a tab bar at the bottom.
Documentation: [CupertinoTabScaffold](https://api.flutter.dev/flutter/cupertino/CupertinoTabScaffold-class.html)

**Constructor:**
```dart
CupertinoTabScaffold({
  Key? key,
  required CupertinoTabBar tabBar,
  required IndexedWidgetBuilder tabBuilder,
  CupertinoTabController? controller,
  Color? backgroundColor,
  bool resizeToAvoidBottomInset = true,
  String? restorationId,
})
```

### CupertinoTextField
Short description: An iOS-style text field.
Documentation: [CupertinoTextField](https://api.flutter.dev/flutter/cupertino/CupertinoTextField-class.html)

**Constructor:**
```dart
CupertinoTextField({
  Key? key,
  TextEditingController? controller,
  FocusNode? focusNode,
  BoxDecoration? decoration = const BoxDecoration(border: Border.fromBorderSide(BorderSide(width: 0.0, color: CupertinoColors.inactiveGray)), borderRadius: BorderRadius.all(Radius.circular(5.0))),
  EdgeInsetsGeometry padding = const EdgeInsets.all(6.0),
  String? placeholder,
  TextStyle? placeholderStyle = const TextStyle(fontWeight: FontWeight.w400, color: CupertinoColors.placeholderText),
  Widget? prefix,
  OverlayVisibilityMode prefixMode = OverlayVisibilityMode.always,
  Widget? suffix,
  OverlayVisibilityMode suffixMode = OverlayVisibilityMode.always,
  OverlayVisibilityMode clearButtonMode = OverlayVisibilityMode.never,
  TextInputType? keyboardType,
  TextInputAction? textInputAction,
  TextCapitalization textCapitalization = TextCapitalization.none,
  TextStyle? style,
  StrutStyle? strutStyle,
  TextAlign textAlign = TextAlign.start,
  TextAlignVertical? textAlignVertical,
  bool readOnly = false,
  ToolbarOptions? toolbarOptions,
  bool? showCursor,
  bool autofocus = false,
  String obscuringCharacter = '•',
  bool obscureText = false,
  bool autocorrect = true,
  SmartDashesType? smartDashesType,
  SmartQuotesType? smartQuotesType,
  bool enableSuggestions = true,
  int? maxLines = 1,
  int? minLines,
  bool expands = false,
  int? maxLength,
  MaxLengthEnforcement? maxLengthEnforcement,
  ValueChanged<String>? onChanged,
  VoidCallback? onEditingComplete,
  ValueChanged<String>? onSubmitted,
  List<TextInputFormatter>? inputFormatters,
  bool? enabled,
  double cursorWidth = 2.0,
  double? cursorHeight,
  Radius cursorRadius = const Radius.circular(2.0),
  Color? cursorColor,
  Brightness? keyboardAppearance,
  EdgeInsets scrollPadding = const EdgeInsets.all(20.0),
  DragStartBehavior dragStartBehavior = DragStartBehavior.start,
  bool enableInteractiveSelection = true,
  TextSelectionControls? selectionControls,
  GestureTapCallback? onTap,
  ScrollController? scrollController,
  ScrollPhysics? scrollPhysics,
  Iterable<String>? autofillHints,
  Clip clipBehavior = Clip.hardEdge,
  String? restorationId,
  bool scribbleEnabled = true,
  bool enableIMEPersonalizedLearning = true,
})
```

### CupertinoTimerPicker
Short description: An iOS-style countdown timer picker.
Documentation: [CupertinoTimerPicker](https://api.flutter.dev/flutter/cupertino/CupertinoTimerPicker-class.html)

**Constructor:**
```dart
CupertinoTimerPicker({
  Key? key,
  CupertinoTimerPickerMode mode = CupertinoTimerPickerMode.hms,
  Duration initialTimerDuration = Duration.zero,
  int minuteInterval = 1,
  int secondInterval = 1,
  AlignmentGeometry alignment = Alignment.center,
  Color? backgroundColor,
  double itemExtent = 32.0,
  required ValueChanged<Duration> onTimerDurationChanged,
})
```

## Documentation Maintenance

This documentation was created based on Flutter's official Cupertino widget catalog and API documentation. It focuses on constructor information for Glue UI specification development.

To update this documentation:
```bash
# Visit https://docs.flutter.dev/ui/widgets/cupertino
# Extract constructor signatures from API documentation
# Update this file with new information
```

## Glue UI Integration Notes

This constructor information will be used to create Glue widget bindings for Flutter Cupertino (iOS-style) UI development. The parameter details help define the Glue syntax for creating and configuring Cupertino widgets programmatically.
