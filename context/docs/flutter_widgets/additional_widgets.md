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
Short description: A scrollable widget using slivers.
Documentation: [CustomScrollView](https://api.flutter.dev/flutter/widgets/CustomScrollView-class.html)

**Note:** Implementation exists but requires detailed property extraction.

### SliverGrid
Short description: A sliver that places multiple children in a 2D arrangement.
Documentation: [SliverGrid](https://api.flutter.dev/flutter/widgets/SliverGrid-class.html)

**Note:** Implementation exists but requires detailed property extraction.

### SliverList
Short description: A sliver that places multiple children in a linear array.
Documentation: [SliverList](https://api.flutter.dev/flutter/widgets/SliverList-class.html)

**Note:** Implementation exists but requires detailed property extraction.

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
Short description: A Material Design bottom navigation bar.
Documentation: [BottomNavigationBar](https://api.flutter.dev/flutter/material/BottomNavigationBar-class.html)

**Note:** Implementation exists but requires detailed property extraction.

### UserAccountsDrawerHeader
Short description: A Material Design drawer header for user account information.
Documentation: [UserAccountsDrawerHeader](https://api.flutter.dev/flutter/material/UserAccountsDrawerHeader-class.html)

**Note:** Implementation exists but requires detailed property extraction.

---

## Material Input & Selection Widgets

### DropdownButton
Short description: A Material Design dropdown button.
Documentation: [DropdownButton](https://api.flutter.dev/flutter/material/DropdownButton-class.html)

**Note:** Implementation exists but requires detailed property extraction.

---

## Material Progress & Loading Widgets

### CircularProgressIndicator
Short description: A Material Design circular progress indicator.
Documentation: [CircularProgressIndicator](https://api.flutter.dev/flutter/material/CircularProgressIndicator-class.html)

**Note:** Implementation exists but requires detailed property extraction.

### RefreshIndicator
Short description: A Material Design pull-to-refresh widget.
Documentation: [RefreshIndicator](https://api.flutter.dev/flutter/material/RefreshIndicator-class.html)

**Note:** Implementation exists but requires detailed property extraction.

---

## Material Search Widgets

### SearchBar
Short description: A Material Design search bar.
Documentation: [SearchBar](https://api.flutter.dev/flutter/material/SearchBar-class.html)

**Note:** Implementation exists but requires detailed property extraction.

### SearchAnchor
Short description: An anchor for a Material Design search view.
Documentation: [SearchAnchor](https://api.flutter.dev/flutter/material/SearchAnchor-class.html)

**Note:** Implementation exists but requires detailed property extraction.

---

## Material Chip Variants

### ActionChip  
Short description: A Material Design action chip for representing actions.
Documentation: [ActionChip](https://api.flutter.dev/flutter/material/ActionChip-class.html)

**Note:** Implementation exists but requires detailed property extraction.

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
Short description: A page view for displaying content corresponding to tabs in a TabBar.
Documentation: [TabBarView](https://api.flutter.dev/flutter/material/TabBarView-class.html)

**Note:** Implementation exists but requires detailed property extraction.

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
Short description: A Material Design tooltip.
Documentation: [Tooltip](https://api.flutter.dev/flutter/material/Tooltip-class.html)

**Note:** Implementation exists but requires detailed property extraction.

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

**Fully Documented (with constructors):**
- Core: Center, Padding, ListView, GridView, SingleChildScrollView (5 widgets)
- Material: Drawer (1 widget)

**Identified (requiring constructor details):**
- Core: CustomScrollView, SliverGrid, SliverList (3 widgets)
- Material: 21 widgets (DrawerHeader, BottomNavigationBar, DropdownButton, CircularProgressIndicator, RefreshIndicator, SearchBar, SearchAnchor, ActionChip, ChoiceChip, FilterChip, InputChip, TabBarView, ExpansionTile, ExpansionPanelList, DataTable, Tooltip, Stepper, PopupMenuButton, UserAccountsDrawerHeader, and generic Button)

**Next Steps:**
1. Extract complete constructor information for remaining 24 widgets
2. Integrate this documentation into the main widget documentation files
3. Update verification plan with all widgets
4. Create property verification tables for each widget

---

**Note:** This is a working document. Constructor details marked as "requires detailed property extraction" need to be filled in by reading the corresponding implementation files and Flutter official documentation.
