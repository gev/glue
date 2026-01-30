# Additional Flutter Widgets

This document contains Flutter widgets that are implemented in the source code but were not included in the initial documentation.

**Source:** Extracted from flutter/glue_flutter/lib/src/lib/ui implementation files

---

## Core Layout & Scrolling Widgets

### Center
Short description: Centers a child widget within itself.
Documentation: [Center](https://api.flutter.dev/flutter/widgets/Center-class.html)

**Constructor:**
```dart
Center({
  Key? key,
  double? widthFactor,
  double? heightFactor,
  Widget? child,
})
```

**Implementation Note:** Current implementation uses `child` only (simplified version).

### Padding
Short description: Adds padding around a child widget.
Documentation: [Padding](https://api.flutter.dev/flutter/widgets/Padding-class.html)

**Constructor:**
```dart
Padding({
  Key? key,
  required EdgeInsetsGeometry padding,
  Widget? child,
})
```

**Implemented Properties:**
- key
- padding (EdgeInsetsGeometry)
- child

### ListView
Short description: A scrollable list of widgets arranged linearly.
Documentation: [ListView](https://api.flutter.dev/flutter/widgets/ListView-class.html)

**Constructor:**
```dart
ListView({
  Key? key,
  Axis scrollDirection = Axis.vertical,
  bool reverse = false,
  ScrollController? controller,
  bool? primary,
  ScrollPhysics? physics,
  bool shrinkWrap = false,
  EdgeInsetsGeometry? padding,
  double? itemExtent,
  Widget? prototypeItem,
  bool addAutomaticKeepAlives = true,
  bool addRepaintBoundaries = true,
  bool addSemanticIndexes = true,
  double? cacheExtent,
  List<Widget> children = const <Widget>[],
  int? semanticChildCount,
  Clip clipBehavior = Clip.hardEdge,
})
```

### GridView
Short description: A scrollable 2D array of widgets.
Documentation: [GridView](https://api.flutter.dev/flutter/widgets/GridView-class.html)

**Constructor:**
```dart
GridView({
  Key? key,
  Axis scrollDirection = Axis.vertical,
  bool reverse = false,
  ScrollController? controller,
  bool? primary,
  ScrollPhysics? physics,
  bool shrinkWrap = false,
  EdgeInsetsGeometry? padding,
  required SliverGridDelegate gridDelegate,
  bool addAutomaticKeepAlives = true,
  bool addRepaintBoundaries = true,
  bool addSemanticIndexes = true,
  double? cacheExtent,
  List<Widget> children = const <Widget>[],
  int? semanticChildCount,
  Clip clipBehavior = Clip.hardEdge,
})
```

### SingleChildScrollView
Short description: A scrollable widget that works with a single child.
Documentation: [SingleChildScrollView](https://api.flutter.dev/flutter/widgets/SingleChildScrollView-class.html)

**Constructor:**
```dart
SingleChildScrollView({
  Key? key,
  Axis scrollDirection = Axis.vertical,
  bool reverse = false,
  EdgeInsetsGeometry? padding,
  bool? primary,
  ScrollPhysics? physics,
  ScrollController? controller,
  DragStartBehavior dragStartBehavior = DragStartBehavior.start,
  Clip clipBehavior = Clip.hardEdge,
  String? restorationId,
  ScrollViewKeyboardDismissBehavior keyboardDismissBehavior = ScrollViewKeyboardDismissBehavior.manual,
  Widget? child,
})
```

### CustomScrollView
Short description: A scrollable widget using slivers for advanced scrolling effects.
Documentation: [CustomScrollView](https://api.flutter.dev/flutter/widgets/CustomScrollView-class.html)

**Constructor:**
```dart
CustomScrollView({
  Key? key,
  Axis scrollDirection = Axis.vertical,
  bool reverse = false,
  ScrollController? controller,
  bool? primary,
  ScrollPhysics? physics,
  bool shrinkWrap = false,
  Key? center,
  double anchor = 0.0,
  double? cacheExtent,
  List<Widget> slivers = const <Widget>[],
  int? semanticChildCount,
  DragStartBehavior dragStartBehavior = DragStartBehavior.start,
  ScrollViewKeyboardDismissBehavior keyboardDismissBehavior = ScrollViewKeyboardDismissBehavior.manual,
  String? restorationId,
  Clip clipBehavior = Clip.hardEdge,
})
```

### SliverGrid
Short description: A sliver that places multiple children in a 2D arrangement for use in CustomScrollView.
Documentation: [SliverGrid](https://api.flutter.dev/flutter/widgets/SliverGrid-class.html)

**Constructor:**
```dart
SliverGrid({
  Key? key,
  required SliverChildDelegate delegate,
  required SliverGridDelegate gridDelegate,
})
```

**Implemented Properties:**
- key
- delegate (SliverChildDelegate) - Note: Implementation uses 'sliver-grid-delegate'
- gridDelegate (SliverGridDelegate) - Note: Implementation uses 'sliver-grid-grid-delegate'

### SliverList
Short description: A sliver that places multiple children in a linear array for use in CustomScrollView.
Documentation: [SliverList](https://api.flutter.dev/flutter/widgets/SliverList-class.html)

**Constructor:**
```dart
SliverList({
  Key? key,
  required SliverChildDelegate delegate,
})
```

**Implemented Properties:**
- key
- delegate (SliverChildDelegate) - Note: Implementation uses 'sliver-list-delegate'

---

## Material Navigation Widgets

### Drawer
Short description: A Material Design panel that slides from the edge of a Scaffold.
Documentation: [Drawer](https://api.flutter.dev/flutter/material/Drawer-class.html)

**Constructor:**
```dart
Drawer({
  Key? key,
  Color? backgroundColor,
  double? elevation,
  Color? shadowColor,
  Color? surfaceTintColor,
  ShapeBorder? shape,
  double? width,
  Clip? clipBehavior = Clip.none,
  String? semanticLabel,
  Widget? child,
})
```

### DrawerHeader
Short description: A Material Design drawer header.
Documentation: [DrawerHeader](https://api.flutter.dev/flutter/material/DrawerHeader-class.html)

**Note:** Implementation exists but requires detailed property extraction.

### BottomNavigationBar
Short description: A Material Design bottom navigation bar for switching between views.
Documentation: [BottomNavigationBar](https://api.flutter.dev/flutter/material/BottomNavigationBar-class.html)

**Constructor:**
```dart
BottomNavigationBar({
  Key? key,
  required List<BottomNavigationBarItem> items,
  ValueChanged<int>? onTap,
  int currentIndex = 0,
  double elevation = 8.0,
  BottomNavigationBarType? type,
  Color? fixedColor,
  Color? backgroundColor,
  double iconSize = 24.0,
  Color? selectedItemColor,
  Color? unselectedItemColor,
  IconThemeData? selectedIconTheme,
  IconThemeData? unselectedIconTheme,
  TextStyle? selectedLabelStyle,
  TextStyle? unselectedLabelStyle,
  double selectedFontSize = 14.0,
  double unselectedFontSize = 12.0,
  bool? showSelectedLabels,
  bool? showUnselectedLabels,
  MouseCursor? mouseCursor,
  bool? enableFeedback,
  BottomNavigationBarLandscapeLayout? landscapeLayout,
  bool useLegacyColorScheme = true,
})
```

### UserAccountsDrawerHeader
Short description: A Material Design drawer header for user account information.
Documentation: [UserAccountsDrawerHeader](https://api.flutter.dev/flutter/material/UserAccountsDrawerHeader-class.html)

**Note:** Implementation exists but requires detailed property extraction.

---

## Material Input & Selection Widgets

### DropdownButton
Short description: A Material Design dropdown button for selecting from a list of items.
Documentation: [DropdownButton](https://api.flutter.dev/flutter/material/DropdownButton-class.html)

**Constructor:**
```dart
DropdownButton<T>({
  Key? key,
  required List<DropdownMenuItem<T>>? items,
  DropdownButtonBuilder? selectedItemBuilder,
  T? value,
  Widget? hint,
  Widget? disabledHint,
  required ValueChanged<T?>? onChanged,
  VoidCallback? onTap,
  int elevation = 8,
  TextStyle? style,
  Widget? underline,
  Widget? icon,
  Color? iconDisabledColor,
  Color? iconEnabledColor,
  double iconSize = 24.0,
  bool isDense = false,
  bool isExpanded = false,
  double? itemHeight = 48.0,
  Color? focusColor,
  FocusNode? focusNode,
  bool autofocus = false,
  Color? dropdownColor,
  double? menuMaxHeight,
  bool? enableFeedback,
  AlignmentGeometry alignment = AlignmentDirectional.centerStart,
  BorderRadius? borderRadius,
  EdgeInsetsGeometry? padding,
})
```

---

## Material Progress & Loading Widgets

### CircularProgressIndicator
Short description: A Material Design circular progress indicator showing determinate or indeterminate progress.
Documentation: [CircularProgressIndicator](https://api.flutter.dev/flutter/material/CircularProgressIndicator-class.html)

**Constructor:**
```dart
CircularProgressIndicator({
  Key? key,
  double? value,
  Color? backgroundColor,
  Color? color,
  Animation<Color?>? valueColor,
  double strokeWidth = 4.0,
  double? strokeAlign,
  StrokeCap? strokeCap,
  String? semanticsLabel,
  String? semanticsValue,
})
```

### RefreshIndicator
Short description: A Material Design pull-to-refresh widget for triggering async refresh operations.
Documentation: [RefreshIndicator](https://api.flutter.dev/flutter/material/RefreshIndicator-class.html)

**Constructor:**
```dart
RefreshIndicator({
  Key? key,
  required Widget child,
  double displacement = 40.0,
  double edgeOffset = 0.0,
  required RefreshCallback onRefresh,
  Color? color,
  Color? backgroundColor,
  ScrollNotificationPredicate notificationPredicate = defaultScrollNotificationPredicate,
  String? semanticsLabel,
  String? semanticsValue,
  double strokeWidth = RefreshProgressIndicator.defaultStrokeWidth,
  RefreshIndicatorTriggerMode triggerMode = RefreshIndicatorTriggerMode.onEdge,
})
```

---

## Material Search Widgets

### SearchBar
Short description: A Material Design search bar for text search input.
Documentation: [SearchBar](https://api.flutter.dev/flutter/material/SearchBar-class.html)

**Constructor:**
```dart
SearchBar({
  Key? key,
  TextEditingController? controller,
  FocusNode? focusNode,
  String? hintText,
  GestureTapCallback? onTap,
  ValueChanged<String>? onChanged,
  ValueChanged<String>? onSubmitted,
  Widget? leading,
  Iterable<Widget>? trailing,
  WidgetStateProperty<double?>? elevation,
  WidgetStateProperty<Color?>? backgroundColor,
  WidgetStateProperty<Color?>? shadowColor,
  WidgetStateProperty<Color?>? surfaceTintColor,
  WidgetStateProperty<Color?>? overlayColor,
  WidgetStateProperty<BorderSide?>? side,
  WidgetStateProperty<OutlinedBorder?>? shape,
  WidgetStateProperty<EdgeInsetsGeometry?>? padding,
  WidgetStateProperty<TextStyle?>? textStyle,
  WidgetStateProperty<TextStyle?>? hintStyle,
  BoxConstraints? constraints,
  TextCapitalization textCapitalization = TextCapitalization.none,
  TextInputAction? textInputAction,
  TextInputType? keyboardType,
  TapRegionCallback? onTapOutside,
  bool enabled = true,
  bool autoFocus = false,
})
```

**Note:** Current implementation uses simplified version with basic properties.

### SearchAnchor
Short description: An anchor for a Material Design search view.
Documentation: [SearchAnchor](https://api.flutter.dev/flutter/material/SearchAnchor-class.html)

**Note:** Implementation exists but requires detailed property extraction.

---

## Material Chip Variants

### ActionChip  
Short description: A Material Design action chip for triggering actions.
Documentation: [ActionChip](https://api.flutter.dev/flutter/material/ActionChip-class.html)

**Constructor:**
```dart
ActionChip({
  Key? key,
  Widget? avatar,
  required Widget label,
  TextStyle? labelStyle,
  EdgeInsetsGeometry? labelPadding,
  VoidCallback? onPressed,
  double? pressElevation,
  String? tooltip,
  BorderSide? side,
  OutlinedBorder? shape,
  Clip clipBehavior = Clip.none,
  FocusNode? focusNode,
  bool autofocus = false,
  Color? backgroundColor,
  EdgeInsetsGeometry? padding,
  VisualDensity? visualDensity,
  MaterialTapTargetSize? materialTapTargetSize,
  double? elevation,
  Color? shadowColor,
  Color? surfaceTintColor,
  IconThemeData? iconTheme,
  Color? disabledColor,
  BoxConstraints? avatarBoxConstraints,
})
```

### ChoiceChip
Short description: A Material Design choice chip for single selection.
Documentation: [ChoiceChip](https://api.flutter.dev/flutter/material/ChoiceChip-class.html)

**Note:** Implementation exists but requires detailed property extraction.

### FilterChip
Short description: A Material Design filter chip for filtering content.
Documentation: [FilterChip](https://api.flutter.dev/flutter/material/FilterChip-class.html)

**Note:** Implementation exists but requires detailed property extraction.

### InputChip
Short description: A Material Design input chip for representing complex information.
Documentation: [InputChip](https://api.flutter.dev/flutter/material/InputChip-class.html)

**Note:** Implementation exists but requires detailed property extraction.

---

## Material Tab Widgets

### TabBarView
Short description: A page view that displays content corresponding to the selected tab in a TabBar.
Documentation: [TabBarView](https://api.flutter.dev/flutter/material/TabBarView-class.html)

**Constructor:**
```dart
TabBarView({
  Key? key,
  required List<Widget> children,
  TabController? controller,
  ScrollPhysics? physics,
  DragStartBehavior dragStartBehavior = DragStartBehavior.start,
  double viewportFraction = 1.0,
  Clip clipBehavior = Clip.hardEdge,
})
```

---

## Material Expansion & Container Widgets

### ExpansionTile
Short description: A Material Design expansion tile for expandable lists.
Documentation: [ExpansionTile](https://api.flutter.dev/flutter/material/ExpansionTile-class.html)

**Note:** Implementation exists but requires detailed property extraction.

### ExpansionPanelList
Short description: A Material Design expansion panel list.
Documentation: [ExpansionPanelList](https://api.flutter.dev/flutter/material/ExpansionPanelList-class.html)

**Note:** Implementation exists but requires detailed property extraction.

---

## Material Data & Display Widgets

### DataTable
Short description: A Material Design data table.
Documentation: [DataTable](https://api.flutter.dev/flutter/material/DataTable-class.html)

**Note:** Implementation exists but requires detailed property extraction.

### Tooltip
Short description: A Material Design tooltip that displays helpful text when widget is long-pressed or hovered.
Documentation: [Tooltip](https://api.flutter.dev/flutter/material/Tooltip-class.html)

**Constructor:**
```dart
Tooltip({
  Key? key,
  String? message,
  InlineSpan? richMessage,
  double? height,
  EdgeInsetsGeometry? padding,
  EdgeInsetsGeometry? margin,
  double? verticalOffset,
  bool? preferBelow,
  bool? excludeFromSemantics,
  Decoration? decoration,
  TextStyle? textStyle,
  TextAlign? textAlign,
  Duration? waitDuration,
  Duration? showDuration,
  Duration? exitDuration,
  TooltipTriggerMode? triggerMode,
  bool? enableFeedback,
  TooltipTriggeredCallback? onTriggered,
  Widget? child,
})
```

**Note:** Implementation uses 'tooltip-' prefix for properties (e.g., 'tooltip-message', 'tooltip-padding').

---

## Material Form & Navigation Widgets

### Stepper
Short description: A Material Design stepper widget for step-by-step processes.
Documentation: [Stepper](https://api.flutter.dev/flutter/material/Stepper-class.html)

**Note:** Implementation exists but requires detailed property extraction.

### PopupMenuButton
Short description: A Material Design popup menu button.
Documentation: [PopupMenuButton](https://api.flutter.dev/flutter/material/PopupMenuButton-class.html)

**Note:** Implementation exists but requires detailed property extraction.

---

## Documentation Status

This file documents **30 additional widgets** found in the implementation:

**Fully Documented (with complete constructors) - 15 widgets:**
- **Core (8):** Center, Padding, ListView, GridView, SingleChildScrollView, CustomScrollView, SliverGrid, SliverList
- **Material (7):** Drawer, BottomNavigationBar, DropdownButton, CircularProgressIndicator, RefreshIndicator, SearchBar, ActionChip, TabBarView, Tooltip

**Partially Documented (identified, needs extraction) - 15 widgets:**
- **Material (15):** DrawerHeader, UserAccountsDrawerHeader, SearchAnchor, ChoiceChip, FilterChip, InputChip, ExpansionTile, ExpansionPanelList, DataTable, Stepper, PopupMenuButton, Button (generic)

**Next Steps:**
1. Extract complete constructor information for remaining 24 widgets
2. Integrate this documentation into the main widget documentation files
3. Update verification plan with all widgets
4. Create property verification tables for each widget

---

**Note:** This is a working document. Constructor details marked as "requires detailed property extraction" need to be filled in by reading the corresponding implementation files and Flutter official documentation.
