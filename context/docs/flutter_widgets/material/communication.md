# Material Communication Widgets

This section provides constructor signatures and parameter details for Material communication widgets.

## Widget Constructor Reference

### Badge
Short description: Displays a small badge anchored to another widget.
Documentation: [Badge](https://api.flutter.dev/flutter/material/Badge-class.html)

**Constructor:**
```dart
Badge({
  Key? key,
  required Widget child,
  Widget? label,
  Color? backgroundColor,
  Color? textColor,
  TextStyle? textStyle,
  EdgeInsetsGeometry? padding,
  AlignmentGeometry? alignment,
  bool? isLabelVisible,
  bool? largeSize,
  Offset? offset,
  bool? showBadge,
})
```

### LinearProgressIndicator
Short description: Shows linear progress or an indeterminate loading bar.
Documentation: [LinearProgressIndicator](https://api.flutter.dev/flutter/material/LinearProgressIndicator-class.html)

**Constructor:**
```dart
LinearProgressIndicator({
  Key? key,
  double? value,
  Color? backgroundColor,
  Color? color,
  Animation<Color>? valueColor,
  double? minHeight,
  String? semanticsLabel,
  String? semanticsValue,
})
```

### SnackBar
Short description: Displays a transient message at the bottom of the screen.
Documentation: [SnackBar](https://api.flutter.dev/flutter/material/SnackBar-class.html)

**Constructor:**
```dart
SnackBar({
  Key? key,
  required Widget content,
  Color? backgroundColor,
  double? elevation,
  EdgeInsetsGeometry? margin,
  EdgeInsetsGeometry? padding,
  double? width,
  ShapeBorder? shape,
  SnackBarBehavior? behavior,
  SnackBarAction? action,
  Duration? duration,
  Animation<double>? animation,
  VoidCallback? onVisible,
  DismissDirection? dismissDirection,
  Clip clipBehavior = Clip.hardEdge,
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

This constructor information will be used to create Glue widget bindings for Flutter Material UI development. The parameter details help define the Glue syntax for creating and configuring Material communication widgets programmatically.
