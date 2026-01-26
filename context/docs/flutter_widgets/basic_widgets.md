# Basic Widgets Constructor Information

This section provides constructor signatures and parameter details for essential Flutter widgets.

Source documentation:
- https://docs.flutter.dev/ui/widgets
- https://api.flutter.dev/flutter/widgets/widgets-library.html

## Widget Constructor Reference

### AppBar
Short description: A Material Design app bar for titles, actions, and navigation controls.
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
  TextStyle? titleTextStyle,
  bool primary = true,
  bool? centerTitle,
  bool excludeHeaderSemantics = false,
  double? titleSpacing,
  double toolbarOpacity = 1.0,
  double bottomOpacity = 1.0,
  double? toolbarHeight,
  double? leadingWidth,
  TextStyle? toolbarTextStyle,
  SystemUiOverlayStyle? systemOverlayStyle,
  bool forceMaterialTransparency = false,
  bool useDefaultSemanticsOrder = true,
  Clip? clipBehavior,
  EdgeInsetsGeometry? actionsPadding,
  bool animateColor = false,
})
```

### Column
Short description: Lays out children in a vertical array.
Documentation: [Column](https://api.flutter.dev/flutter/widgets/Column-class.html)

**Constructor:**
```dart
Column({
  Key? key,
  MainAxisAlignment mainAxisAlignment = MainAxisAlignment.start,
  MainAxisSize mainAxisSize = MainAxisSize.max,
  CrossAxisAlignment crossAxisAlignment = CrossAxisAlignment.center,
  TextDirection? textDirection,
  VerticalDirection verticalDirection = VerticalDirection.down,
  TextBaseline? textBaseline,
  List<Widget> children = const <Widget>[],
})
```

### Container
Short description: A convenience widget that combines painting, positioning, and sizing.
Documentation: [Container](https://api.flutter.dev/flutter/widgets/Container-class.html)

**Constructor:**
```dart
Container({
  Key? key,
  AlignmentGeometry? alignment,
  EdgeInsetsGeometry? padding,
  Color? color,
  Decoration? decoration,
  Decoration? foregroundDecoration,
  double? width,
  double? height,
  BoxConstraints? constraints,
  EdgeInsetsGeometry? margin,
  Matrix4? transform,
  AlignmentGeometry? transformAlignment,
  Widget? child,
  Clip clipBehavior = Clip.none,
})
```

### ElevatedButton
Short description: A Material button with elevation and fill color.
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

### FlutterLogo
Short description: The Flutter logo with customizable style and animation.
Documentation: [FlutterLogo](https://api.flutter.dev/flutter/widgets/FlutterLogo-class.html)

**Constructor:**
```dart
FlutterLogo({
  Key? key,
  double size = 100.0,
  FlutterLogoStyle style = FlutterLogoStyle.markOnly,
  Color? textColor,
  Color? colors,
  Duration duration = const Duration(milliseconds: 750),
  Curve curve = Curves.fastOutSlowIn,
})
```

### Icon
Short description: Draws a glyph from a font described in an IconData.
Documentation: [Icon](https://api.flutter.dev/flutter/widgets/Icon-class.html)

**Constructor:**
```dart
Icon({
  Key? key,
  required IconData icon,
  double? size,
  Color? color,
  String? semanticLabel,
  TextDirection? textDirection,
})
```

### Image
Short description: Displays an image from an ImageProvider.
Documentation: [Image](https://api.flutter.dev/flutter/widgets/Image-class.html)

**Constructor:**
```dart
Image({
  Key? key,
  required ImageProvider image,
  double? width,
  double? height,
  Color? color,
  BlendMode? colorBlendMode,
  BoxFit? fit,
  AlignmentGeometry alignment = Alignment.center,
  ImageRepeat repeat = ImageRepeat.noRepeat,
  Rect? centerSlice,
  bool matchTextDirection = false,
  bool gaplessPlayback = false,
  String? semanticLabel,
  bool excludeFromSemantics = false,
  FilterQuality filterQuality = FilterQuality.low,
  int? cacheWidth,
  int? cacheHeight,
})
```

### Placeholder
Short description: A box that represents a widget that will be added later.
Documentation: [Placeholder](https://api.flutter.dev/flutter/widgets/Placeholder-class.html)

**Constructor:**
```dart
Placeholder({
  Key? key,
  double? fallbackWidth,
  double? fallbackHeight,
  Color? color,
  StrokeAlign strokeAlign = StrokeAlign.inside,
  double strokeWidth = 2.0,
})
```

### Row
Short description: Lays out children in a horizontal array.
Documentation: [Row](https://api.flutter.dev/flutter/widgets/Row-class.html)

**Constructor:**
```dart
Row({
  Key? key,
  MainAxisAlignment mainAxisAlignment = MainAxisAlignment.start,
  MainAxisSize mainAxisSize = MainAxisSize.max,
  CrossAxisAlignment crossAxisAlignment = CrossAxisAlignment.center,
  TextDirection? textDirection,
  VerticalDirection verticalDirection = VerticalDirection.down,
  TextBaseline? textBaseline,
  List<Widget> children = const <Widget>[],
})
```

### Scaffold
Short description: Implements the basic Material Design visual layout structure.
Documentation: [Scaffold](https://api.flutter.dev/flutter/material/Scaffold-class.html)

**Constructor:**
```dart
Scaffold({
  Key? key,
  PreferredSizeWidget? appBar,
  Widget? body,
  Widget? floatingActionButton,
  FloatingActionButtonLocation? floatingActionButtonLocation,
  Widget? floatingActionButtonAnimator,
  Widget? persistentFooterButtons,
  Widget? drawer,
  Widget? endDrawer,
  Widget? bottomNavigationBar,
  Widget? bottomSheet,
  Color? backgroundColor,
  bool? resizeToAvoidBottomInset,
  bool primary = true,
  DragStartBehavior drawerDragStartBehavior = DragStartBehavior.start,
  bool extendBody = false,
  bool extendBodyBehindAppBar = false,
  Color? drawerScrimColor,
  double? drawerEdgeDragWidth,
  bool drawerEnableOpenDragGesture = true,
  bool endDrawerEnableOpenDragGesture = true,
  String? restorationId,
})
```

### Text
Short description: Displays a string of text with a single style.
Documentation: [Text](https://api.flutter.dev/flutter/widgets/Text-class.html)

**Constructor:**
```dart
Text({
  Key? key,
  required String data,
  TextStyle? style,
  StrutStyle? strutStyle,
  TextAlign? textAlign,
  TextDirection? textDirection,
  Locale? locale,
  bool? softWrap,
  TextOverflow? overflow,
  double? textScaleFactor,
  int? maxLines,
  String? semanticsLabel,
  TextWidthBasis? textWidthBasis,
  TextHeightBehavior? textHeightBehavior,
})
```

## Documentation Maintenance

This documentation was extracted from Flutter's official API documentation and is focused on constructor information for Glue UI specification development.

To update this documentation:
```bash
# Fetch latest widget API documentation
# Extract constructor signatures and parameter details
# Update this README with new information
```

## Glue UI Integration Notes

This constructor information will be used to create Glue widget bindings for Flutter UI development. The parameter details help define the Glue syntax for creating and configuring Flutter widgets programmatically.
