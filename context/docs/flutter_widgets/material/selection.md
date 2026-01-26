# Material Selection Widgets

This section provides constructor signatures and parameter details for Material selection widgets.

## Widget Constructor Reference

### Checkbox
Short description: A Material checkbox for boolean input.
Documentation: [Checkbox](https://api.flutter.dev/flutter/material/Checkbox-class.html)

**Constructor:**
```dart
Checkbox({
  Key? key,
  required bool? value,
  bool tristate = false,
  required ValueChanged<bool?>? onChanged,
  MouseCursor? mouseCursor,
  Color? activeColor,
  WidgetStateProperty<Color?>? fillColor,
  Color? checkColor,
  Color? focusColor,
  Color? hoverColor,
  WidgetStateProperty<Color?>? overlayColor,
  double? splashRadius,
  MaterialTapTargetSize? materialTapTargetSize,
  VisualDensity? visualDensity,
  FocusNode? focusNode,
  bool autofocus = false,
  OutlinedBorder? shape,
  BorderSide? side,
  bool isError = false,
  String? semanticLabel,
})
```

### Chip
Short description: A compact element representing an attribute, action, or choice.
Documentation: [Chip](https://api.flutter.dev/flutter/material/Chip-class.html)

**Constructor:**
```dart
Chip({
  Key? key,
  Widget? avatar,
  required Widget label,
  TextStyle? labelStyle,
  EdgeInsetsGeometry? labelPadding,
  Widget? deleteIcon,
  VoidCallback? onDeleted,
  Color? deleteIconColor,
  String? deleteButtonTooltipMessage,
  BorderSide? side,
  OutlinedBorder? shape,
  Clip clipBehavior = Clip.none,
  FocusNode? focusNode,
  bool autofocus = false,
  WidgetStateProperty<Color?>? color,
  Color? backgroundColor,
  EdgeInsetsGeometry? padding,
  VisualDensity? visualDensity,
  MaterialTapTargetSize? materialTapTargetSize,
  double? elevation,
  Color? shadowColor,
  Color? surfaceTintColor,
  IconThemeData? iconTheme,
  BoxConstraints? avatarBoxConstraints,
  BoxConstraints? deleteIconBoxConstraints,
  ChipAnimationStyle? chipAnimationStyle,
  MouseCursor? mouseCursor,
})
```

### DatePickerDialog
Short description: A dialog that lets users select a date.
Documentation: [DatePickerDialog](https://api.flutter.dev/flutter/material/DatePickerDialog-class.html)

**Constructor:**
```dart
DatePickerDialog({
  Key? key,
  DateTime? initialDate,
  required DateTime firstDate,
  required DateTime lastDate,
  DateTime? currentDate,
  DatePickerEntryMode initialEntryMode = DatePickerEntryMode.calendar,
  SelectableDayPredicate? selectableDayPredicate,
  String? cancelText,
  String? confirmText,
  String? helpText,
  DatePickerMode initialCalendarMode = DatePickerMode.day,
  String? errorFormatText,
  String? errorInvalidText,
  String? fieldHintText,
  String? fieldLabelText,
  TextInputType? keyboardType,
  String? restorationId,
  ValueChanged<DatePickerEntryMode>? onDatePickerModeChange,
  Icon? switchToInputEntryModeIcon,
  Icon? switchToCalendarEntryModeIcon,
  EdgeInsets insetPadding = const EdgeInsets.symmetric(horizontal: 16.0, vertical: 24.0),
  CalendarDelegate<DateTime> calendarDelegate = const GregorianCalendarDelegate(),
})
```

### MenuAnchor
Short description: Anchors a Material menu to a widget or location.
Documentation: [MenuAnchor](https://api.flutter.dev/flutter/material/MenuAnchor-class.html)

**Constructor:**
```dart
MenuAnchor({
  Key? key,
  MenuController? controller,
  FocusNode? childFocusNode,
  MenuStyle? style,
  Offset? alignmentOffset = Offset.zero,
  EdgeInsetsGeometry? reservedPadding,
  LayerLink? layerLink,
  Clip clipBehavior = Clip.hardEdge,
  @Deprecated('Use consumeOutsideTap instead. This feature was deprecated after v3.16.0-8.0.pre.')
  bool anchorTapClosesMenu = false,
  bool consumeOutsideTap = false,
  VoidCallback? onOpen,
  VoidCallback? onClose,
  bool crossAxisUnconstrained = true,
  bool useRootOverlay = false,
  required List<Widget> menuChildren,
  MenuAnchorChildBuilder? builder,
  Widget? child,
})
```

### Radio
Short description: A Material radio button for mutually exclusive selection.
Documentation: [Radio](https://api.flutter.dev/flutter/material/Radio-class.html)

**Constructor:**
```dart
Radio({
  Key? key,
  required T value,
  @Deprecated('Use a RadioGroup ancestor to manage group value instead. This feature was deprecated after v3.32.0-0.0.pre.')
  T? groupValue,
  @Deprecated('Use RadioGroup to handle value change instead. This feature was deprecated after v3.32.0-0.0.pre.')
  ValueChanged<T?>? onChanged,
  MouseCursor? mouseCursor,
  bool toggleable = false,
  Color? activeColor,
  WidgetStateProperty<Color?>? fillColor,
  Color? focusColor,
  Color? hoverColor,
  WidgetStateProperty<Color?>? overlayColor,
  double? splashRadius,
  MaterialTapTargetSize? materialTapTargetSize,
  VisualDensity? visualDensity,
  FocusNode? focusNode,
  bool autofocus = false,
  bool? enabled,
  RadioGroupRegistry<T>? groupRegistry,
  WidgetStateProperty<Color?>? backgroundColor,
  BorderSide? side,
  WidgetStateProperty<double?>? innerRadius,
})
```

### Slider
Short description: A Material slider for selecting from a range of values.
Documentation: [Slider](https://api.flutter.dev/flutter/material/Slider-class.html)

**Constructor:**
```dart
Slider({
  Key? key,
  required double value,
  double? secondaryTrackValue,
  required ValueChanged<double>? onChanged,
  ValueChanged<double>? onChangeStart,
  ValueChanged<double>? onChangeEnd,
  double min = 0.0,
  double max = 1.0,
  int? divisions,
  String? label,
  Color? activeColor,
  Color? inactiveColor,
  Color? secondaryActiveColor,
  Color? thumbColor,
  WidgetStateProperty<Color?>? overlayColor,
  MouseCursor? mouseCursor,
  SemanticFormatterCallback? semanticFormatterCallback,
  FocusNode? focusNode,
  bool autofocus = false,
  SliderInteraction? allowedInteraction,
  EdgeInsetsGeometry? padding,
  @Deprecated('Set this flag to false to opt into the 2024 slider appearance. Defaults to true. In the future, this flag will default to false. Use SliderThemeData to customize individual properties. This feature was deprecated after v3.27.0-0.2.pre.')
  bool? year2023,
})
```

### Switch
Short description: A Material switch for toggling a boolean state.
Documentation: [Switch](https://api.flutter.dev/flutter/material/Switch-class.html)

**Constructor:**
```dart
Switch({
  Key? key,
  required bool value,
  required ValueChanged<bool>? onChanged,
  @Deprecated('Use activeThumbColor instead. This feature was deprecated after v3.31.0-2.0.pre.')
  Color? activeColor,
  Color? activeThumbColor,
  Color? activeTrackColor,
  Color? inactiveThumbColor,
  Color? inactiveTrackColor,
  ImageProvider<Object>? activeThumbImage,
  ImageErrorListener? onActiveThumbImageError,
  ImageProvider<Object>? inactiveThumbImage,
  ImageErrorListener? onInactiveThumbImageError,
  WidgetStateProperty<Color?>? thumbColor,
  WidgetStateProperty<Color?>? trackColor,
  WidgetStateProperty<Color?>? trackOutlineColor,
  WidgetStateProperty<double?>? trackOutlineWidth,
  WidgetStateProperty<Icon?>? thumbIcon,
  MaterialTapTargetSize? materialTapTargetSize,
  DragStartBehavior dragStartBehavior = DragStartBehavior.start,
  MouseCursor? mouseCursor,
  Color? focusColor,
  Color? hoverColor,
  WidgetStateProperty<Color?>? overlayColor,
  double? splashRadius,
  FocusNode? focusNode,
  ValueChanged<bool>? onFocusChange,
  bool autofocus = false,
  EdgeInsetsGeometry? padding,
})
```

### TimePickerDialog
Short description: A dialog that lets users select a time.
Documentation: [TimePickerDialog](https://api.flutter.dev/flutter/material/TimePickerDialog-class.html)

**Constructor:**
```dart
TimePickerDialog({
  Key? key,
  required TimeOfDay initialTime,
  String? cancelText,
  String? confirmText,
  String? helpText,
  String? errorInvalidText,
  String? hourLabelText,
  String? minuteLabelText,
  String? restorationId,
  TimePickerEntryMode initialEntryMode = TimePickerEntryMode.dial,
  Orientation? orientation,
  EntryModeChangeCallback? onEntryModeChanged,
  Icon? switchToInputEntryModeIcon,
  Icon? switchToTimerEntryModeIcon,
  bool emptyInitialInput = false,
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

This constructor information will be used to create Glue widget bindings for Flutter Material UI development. The parameter details help define the Glue syntax for creating and configuring Material selection widgets programmatically.
