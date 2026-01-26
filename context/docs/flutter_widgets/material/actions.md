# Material Actions Widgets

This section provides constructor signatures and parameter details for Material action widgets.

## Widget Constructor Reference

### Common Buttons

#### ElevatedButton
Short description: A Material button with elevation, used for primary actions.
Documentation: [ElevatedButton](https://api.flutter.dev/flutter/material/ElevatedButton-class.html)

**Constructor:**
```dart
ElevatedButton({
  Key? key,
  required VoidCallback? onPressed,
  VoidCallback? onLongPress,
  ValueChanged<bool>? onHover,
  ValueChanged<bool>? onFocusChange,
  ButtonStyle? style,
  FocusNode? focusNode,
  bool autofocus = false,
  Clip clipBehavior = Clip.none,
  required Widget child,
})
```

#### FilledButton
Short description: A filled Material button for high-emphasis actions.
Documentation: [FilledButton](https://api.flutter.dev/flutter/material/FilledButton-class.html)

**Constructor:**
```dart
FilledButton({
  Key? key,
  required VoidCallback? onPressed,
  VoidCallback? onLongPress,
  ValueChanged<bool>? onHover,
  ValueChanged<bool>? onFocusChange,
  ButtonStyle? style,
  FocusNode? focusNode,
  bool autofocus = false,
  Clip clipBehavior = Clip.none,
  required Widget child,
})
```

#### OutlinedButton
Short description: A button with an outline, used for medium-emphasis actions.
Documentation: [OutlinedButton](https://api.flutter.dev/flutter/material/OutlinedButton-class.html)

**Constructor:**
```dart
OutlinedButton({
  Key? key,
  required VoidCallback? onPressed,
  VoidCallback? onLongPress,
  ValueChanged<bool>? onHover,
  ValueChanged<bool>? onFocusChange,
  ButtonStyle? style,
  FocusNode? focusNode,
  bool autofocus = false,
  Clip clipBehavior = Clip.none,
  required Widget child,
})
```

#### TextButton
Short description: A text-only Material button for low-emphasis actions.
Documentation: [TextButton](https://api.flutter.dev/flutter/material/TextButton-class.html)

**Constructor:**
```dart
TextButton({
  Key? key,
  required VoidCallback? onPressed,
  VoidCallback? onLongPress,
  ValueChanged<bool>? onHover,
  ValueChanged<bool>? onFocusChange,
  ButtonStyle? style,
  FocusNode? focusNode,
  bool autofocus = false,
  Clip clipBehavior = Clip.none,
  required Widget child,
})
```

### FloatingActionButton
Short description: A circular button for primary actions in a scaffold.
Documentation: [FloatingActionButton](https://api.flutter.dev/flutter/material/FloatingActionButton-class.html)

**Constructor:**
```dart
FloatingActionButton({
  Key? key,
  Widget? child,
  String? tooltip,
  Color? foregroundColor,
  Color? backgroundColor,
  Color? focusColor,
  Color? hoverColor,
  Color? splashColor,
  Object? heroTag = const Object(),
  double? elevation,
  double? focusElevation,
  double? hoverElevation,
  double? highlightElevation,
  double? disabledElevation,
  bool? mini,
  OutlinedBorder? shape,
  Clip clipBehavior = Clip.none,
  bool isExtended = false,
  bool autofocus = false,
  MaterialTapTargetSize? materialTapTargetSize,
  bool? enableFeedback,
  Alignment? alignment,
  Offset? offset,
  VoidCallback? onPressed,
  VoidCallback? onLongPress,
  MouseCursor? mouseCursor,
  FocusNode? focusNode,
  String? restorationId,
})
```

### Extended FloatingActionButton
Short description: A floating action button that includes a label and optional icon.
Documentation: [FloatingActionButton.extended](https://api.flutter.dev/flutter/material/FloatingActionButton/FloatingActionButton.extended.html)

**Constructor:**
```dart
FloatingActionButton.extended({
  Key? key,
  required Widget label,
  required VoidCallback? onPressed,
  VoidCallback? onLongPress,
  ValueChanged<bool>? onHover,
  ValueChanged<bool>? onFocusChange,
  ButtonStyle? style,
  FocusNode? focusNode,
  bool autofocus = false,
  Clip clipBehavior = Clip.none,
  Widget? icon,
})
```

### IconButton
Short description: A clickable icon button with configurable visual states.
Documentation: [IconButton](https://api.flutter.dev/flutter/material/IconButton-class.html)

**Constructor:**
```dart
IconButton({
  Key? key,
  Widget? icon,
  Color? color,
  Color? focusColor,
  Color? hoverColor,
  Color? highlightColor,
  Color? splashColor,
  Color? disabledColor,
  double iconSize = 24.0,
  VisualDensity? visualDensity,
  EdgeInsetsGeometry padding = const EdgeInsets.all(8.0),
  AlignmentGeometry alignment = Alignment.center,
  double? splashRadius,
  String? tooltip,
  bool autofocus = false,
  bool? mini,
  VoidCallback? onPressed,
  VoidCallback? onLongPress,
  MouseCursor? mouseCursor,
  FocusNode? focusNode,
  String? restorationId,
})
```

### SegmentedButton
Short description: A set of options presented as segmented controls.
Documentation: [SegmentedButton](https://api.flutter.dev/flutter/material/SegmentedButton-class.html)

**Constructor:**
```dart
SegmentedButton({
  Key? key,
  required Set<T> selected,
  required List<Widget> segments,
  required ValueChanged<Set<T>> onSelectionChanged,
  Set<T>? multiSelectionEnabledFor,
  bool? showSelectedIcon,
  Color? backgroundColor,
  Color? unselectedColor,
  Color? selectedColor,
  Color? disabledColor,
  Color? shadowColor,
  Color? surfaceTintColor,
  double? elevation,
  EdgeInsetsGeometry? padding,
  VisualDensity? visualDensity,
  MaterialTapTargetSize? materialTapTargetSize,
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

This constructor information will be used to create Glue widget bindings for Flutter Material UI development. The parameter details help define the Glue syntax for creating and configuring Material action widgets programmatically.
