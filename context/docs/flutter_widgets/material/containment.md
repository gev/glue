# Material Containment Widgets

This section provides constructor signatures and parameter details for Material containment widgets.

## Widget Constructor Reference

### AlertDialog
Short description: A Material dialog that can display a title, content, and actions.
Documentation: [AlertDialog](https://api.flutter.dev/flutter/material/AlertDialog-class.html)

**Constructor:**
```dart
AlertDialog({
  Key? key,
  Widget? icon,
  EdgeInsetsGeometry? iconPadding,
  Color? iconColor,
  Widget? title,
  EdgeInsetsGeometry? titlePadding,
  TextStyle? titleTextStyle,
  Widget? content,
  EdgeInsetsGeometry? contentPadding,
  TextStyle? contentTextStyle,
  List<Widget>? actions,
  EdgeInsetsGeometry? actionsPadding,
  MainAxisAlignment? actionsAlignment,
  OverflowBarAlignment? actionsOverflowAlignment,
  VerticalDirection? actionsOverflowDirection,
  double? actionsOverflowButtonSpacing,
  EdgeInsetsGeometry? buttonPadding,
  Color? backgroundColor,
  double? elevation,
  Color? shadowColor,
  Color? surfaceTintColor,
  String? semanticLabel,
  EdgeInsets? insetPadding,
  Clip? clipBehavior,
  ShapeBorder? shape,
  AlignmentGeometry? alignment,
  BoxConstraints? constraints,
  bool scrollable = false,
})
```

### BottomSheet
Short description: A persistent or modal sheet that slides up from the bottom.
Documentation: [BottomSheet](https://api.flutter.dev/flutter/material/BottomSheet-class.html)

**Constructor:**
```dart
BottomSheet({
  Key? key,
  AnimationController? animationController,
  bool enableDrag = true,
  bool? showDragHandle,
  Color? dragHandleColor,
  Size? dragHandleSize,
  BottomSheetDragStartHandler? onDragStart,
  BottomSheetDragEndHandler? onDragEnd,
  Color? backgroundColor,
  Color? shadowColor,
  double? elevation,
  ShapeBorder? shape,
  Clip? clipBehavior,
  BoxConstraints? constraints,
  required VoidCallback onClosing,
  required WidgetBuilder builder,
})
```

### Card
Short description: A Material card with rounded corners and elevation.
Documentation: [Card](https://api.flutter.dev/flutter/material/Card-class.html)

**Constructor:**
```dart
Card({
  Key? key,
  Color? color,
  Color? shadowColor,
  Color? surfaceTintColor,
  double? elevation,
  ShapeBorder? shape,
  bool borderOnForeground = true,
  EdgeInsetsGeometry? margin,
  Clip? clipBehavior,
  Widget? child,
  bool semanticContainer = true,
})
```

### Divider
Short description: A horizontal line used to separate content.
Documentation: [Divider](https://api.flutter.dev/flutter/material/Divider-class.html)

**Constructor:**
```dart
Divider({
  Key? key,
  double? height,
  double? thickness,
  double? indent,
  double? endIndent,
  Color? color,
  BorderRadiusGeometry? radius,
})
```

### ListTile
Short description: A single fixed-height row that typically contains text and leading/trailing widgets.
Documentation: [ListTile](https://api.flutter.dev/flutter/material/ListTile-class.html)

**Constructor:**
```dart
ListTile({
  Key? key,
  Widget? leading,
  Widget? title,
  Widget? subtitle,
  Widget? trailing,
  bool? isThreeLine,
  bool? dense,
  VisualDensity? visualDensity,
  ShapeBorder? shape,
  ListTileStyle? style,
  Color? selectedColor,
  Color? iconColor,
  Color? textColor,
  TextStyle? titleTextStyle,
  TextStyle? subtitleTextStyle,
  TextStyle? leadingAndTrailingTextStyle,
  EdgeInsetsGeometry? contentPadding,
  bool enabled = true,
  GestureTapCallback? onTap,
  GestureLongPressCallback? onLongPress,
  ValueChanged<bool>? onFocusChange,
  MouseCursor? mouseCursor,
  bool selected = false,
  Color? focusColor,
  Color? hoverColor,
  Color? splashColor,
  FocusNode? focusNode,
  bool autofocus = false,
  Color? tileColor,
  Color? selectedTileColor,
  bool? enableFeedback,
  double? horizontalTitleGap,
  double? minVerticalPadding,
  double? minLeadingWidth,
  double? minTileHeight,
  ListTileTitleAlignment? titleAlignment,
  bool internalAddSemanticForOnTap = true,
  MaterialStatesController? statesController,
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

This constructor information will be used to create Glue widget bindings for Flutter Material UI development. The parameter details help define the Glue syntax for creating and configuring Material containment widgets programmatically.
