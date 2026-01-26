# Material Navigation Widgets

This section provides constructor signatures and parameter details for Material navigation widgets.

## Widget Constructor Reference

### AppBar
Short description: A Material Design app bar for top-level navigation and actions.
Documentation: [AppBar](https://api.flutter.dev/flutter/material/AppBar-class.html)

**Constructor:**
```dart
AppBar({
  Key? key,
  Widget? leading,
  bool automaticallyImplyLeading = true,
  Widget? title,
  List<Widget>? actions,
  bool automaticallyImplyActions = true,
  Widget? flexibleSpace,
  PreferredSizeWidget? bottom,
  double? elevation,
  double? scrolledUnderElevation,
  ScrollNotificationPredicate notificationPredicate = defaultScrollNotificationPredicate,
  Color? shadowColor,
  Color? surfaceTintColor,
  ShapeBorder? shape,
  Color? backgroundColor,
  Color? foregroundColor,
  IconThemeData? iconTheme,
  IconThemeData? actionsIconTheme,
  bool primary = true,
  bool? centerTitle,
  bool excludeHeaderSemantics = false,
  double? titleSpacing,
  double toolbarOpacity = 1.0,
  double bottomOpacity = 1.0,
  double? toolbarHeight,
  double? leadingWidth,
  TextStyle? toolbarTextStyle,
  TextStyle? titleTextStyle,
  SystemUiOverlayStyle? systemOverlayStyle,
  bool forceMaterialTransparency = false,
  bool useDefaultSemanticsOrder = true,
  Clip? clipBehavior,
  EdgeInsetsGeometry? actionsPadding,
  bool animateColor = false,
})
```

### BottomAppBar
Short description: A bottom app bar that can host navigation and actions.
Documentation: [BottomAppBar](https://api.flutter.dev/flutter/material/BottomAppBar-class.html)

**Constructor:**
```dart
BottomAppBar({
  Key? key,
  Color? color,
  double? elevation,
  NotchedShape? shape,
  Clip clipBehavior = Clip.none,
  double notchMargin = 4.0,
  Widget? child,
  EdgeInsetsGeometry? padding,
  Color? surfaceTintColor,
  Color? shadowColor,
  double? height,
})
```

### NavigationBar
Short description: A Material navigation bar for top-level destinations.
Documentation: [NavigationBar](https://api.flutter.dev/flutter/material/NavigationBar-class.html)

**Constructor:**
```dart
NavigationBar({
  Key? key,
  Duration? animationDuration,
  int selectedIndex = 0,
  required List<Widget> destinations,
  ValueChanged<int>? onDestinationSelected,
  Color? backgroundColor,
  double? elevation,
  Color? shadowColor,
  Color? surfaceTintColor,
  Color? indicatorColor,
  ShapeBorder? indicatorShape,
  double? height,
  NavigationDestinationLabelBehavior? labelBehavior,
  WidgetStateProperty<Color?>? overlayColor,
  WidgetStateProperty<TextStyle?>? labelTextStyle,
  EdgeInsetsGeometry? labelPadding,
  bool maintainBottomViewPadding = false,
})
```

### NavigationDrawer
Short description: A Material drawer that presents navigation destinations.
Documentation: [NavigationDrawer](https://api.flutter.dev/flutter/material/NavigationDrawer-class.html)

**Constructor:**
```dart
NavigationDrawer({
  Key? key,
  required List<Widget> children,
  Widget? header,
  Widget? footer,
  Color? backgroundColor,
  Color? shadowColor,
  Color? surfaceTintColor,
  double? elevation,
  Color? indicatorColor,
  ShapeBorder? indicatorShape,
  ValueChanged<int>? onDestinationSelected,
  int? selectedIndex = 0,
  EdgeInsetsGeometry tilePadding = const EdgeInsets.symmetric(horizontal: 12.0),
})
```

### NavigationRail
Short description: A Material rail for navigation on larger screens.
Documentation: [NavigationRail](https://api.flutter.dev/flutter/material/NavigationRail-class.html)

**Constructor:**
```dart
NavigationRail({
  Key? key,
  Color? backgroundColor,
  bool extended = false,
  Widget? leading,
  Widget? trailing,
  required List<NavigationRailDestination> destinations,
  required int? selectedIndex,
  ValueChanged<int>? onDestinationSelected,
  double? elevation,
  double? groupAlignment,
  NavigationRailLabelType? labelType,
  TextStyle? unselectedLabelTextStyle,
  TextStyle? selectedLabelTextStyle,
  IconThemeData? unselectedIconTheme,
  IconThemeData? selectedIconTheme,
  double? minWidth,
  double? minExtendedWidth,
  bool? useIndicator,
  Color? indicatorColor,
  ShapeBorder? indicatorShape,
  bool leadingAtTop = true,
  bool trailingAtBottom = false,
  bool scrollable = false,
})
```

### TabBar
Short description: A Material tab bar for switching between views.
Documentation: [TabBar](https://api.flutter.dev/flutter/material/TabBar-class.html)

**Constructor:**
```dart
TabBar({
  Key? key,
  required List<Widget> tabs,
  TabController? controller,
  bool isScrollable = false,
  EdgeInsetsGeometry? padding,
  Color? indicatorColor,
  bool automaticIndicatorColorAdjustment = true,
  double indicatorWeight = 2.0,
  EdgeInsetsGeometry indicatorPadding = EdgeInsets.zero,
  Decoration? indicator,
  TabBarIndicatorSize? indicatorSize,
  Color? dividerColor,
  double? dividerHeight,
  Color? labelColor,
  TextStyle? labelStyle,
  EdgeInsetsGeometry? labelPadding,
  Color? unselectedLabelColor,
  TextStyle? unselectedLabelStyle,
  DragStartBehavior dragStartBehavior = DragStartBehavior.start,
  WidgetStateProperty<Color?>? overlayColor,
  MouseCursor? mouseCursor,
  bool? enableFeedback,
  ValueChanged<int>? onTap,
  TabValueChanged<bool>? onHover,
  TabValueChanged<bool>? onFocusChange,
  ScrollPhysics? physics,
  InteractiveInkFeatureFactory? splashFactory,
  BorderRadius? splashBorderRadius,
  TabAlignment? tabAlignment,
  TextScaler? textScaler,
  TabIndicatorAnimation? indicatorAnimation,
})
```

## Documentation Maintenance

This documentation was extracted from Flutter's official API documentation and is focused on constructor information for Glue UI specification development.

To update this documentation:
```bash
# Fetch latest widget API documentation
# Extract constructor signatures and parameter details
# Update this file with new information
```

## Glue UI Integration Notes

This constructor information will be used to create Glue widget bindings for Flutter Material UI development. The parameter details help define the Glue syntax for creating and configuring Material navigation widgets programmatically.
