### Property Access Methods by Type
- **Direct Property Access**: `key`, `child`, `children`, `width`, `height`, `top`, `bottom`, `left`, `right`, `start`, `end`, `horizontal`, `vertical`
  - Access as: `properties.key`, `properties.child`, `properties.children`, etc.
- **bool**: `properties.getBool('property-name')`
- **Color**: `properties.getColor('property-name')`
- **double**: `properties.getDouble('property-name')`
- **int**: `properties.getInt('property-name')`
- **String**: `properties.getString('property-name')`
- **Widget**: `properties.getWidget('property-name')`
- **List\<Widge\>**: `properties.getWidgets('property-name')`
- **Complex/Custom/ScrollController/ValueChanged<T>**: `properties.getValue('property-name')`
- **VoidCallback**: `properties.getVoidCallback('property-name', runtime)` *(requires runtime wrapper)*

### Implementation Requirements
- Always include `key: properties.key` as first constructor parameter
- For widgets with VoidCallback properties, wrap constructor in runtime function:
  ```dart
  Eval<Ir> _createWidget(WidgetProperties properties) {
    return getRuntime().map((runtime) {
      final widget = Constructor(
        key: properties.key,
        onPressed: properties.getVoidCallback('on-pressed', runtime),
        // ... other properties
      );
      return IrNativeValue(Value(widget));
    });
  }
  ```

| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | leading | Widget? | getWidget | leading |
| [x] | title | Widget? | getWidget | title |
| [x] | subtitle | Widget? | getWidget | subtitle |
| [x] | trailing | Widget? | getWidget | trailing |
| [x] | isThreeLine | bool? | getBool | is-three-line |
| [x] | dense | bool? | getBool | dense |
| [x] | visualDensity | VisualDensity? | getValue | visual-density |
| [x] | shape | ShapeBorder? | getValue | shape |
| [x] | style | ListTileStyle? | getValue | style |
| [x] | selectedColor | Color? | getColor | selected-color |
| [x] | iconColor | Color? | getColor | icon-color |
| [x] | textColor | Color? | getColor | text-color |
| [x] | titleTextStyle | TextStyle? | getValue | title-text-style |
| [x] | subtitleTextStyle | TextStyle? | getValue | subtitle-text-style |
| [x] | leadingAndTrailingTextStyle | TextStyle? | getValue | leading-and-trailing-text-style |
| [x] | contentPadding | EdgeInsetsGeometry? | getValue | content-padding |
| [x] | enabled | bool | getBool | enabled |
| [x] | onTap | GestureTapCallback? | getVoidCallback | on-tap |
| [x] | onLongPress | GestureLongPressCallback? | getValue | on-long-press |
| [x] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | selected | bool | getBool | selected |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | hoverColor | Color? | getColor | hover-color |
| [x] | splashColor | Color? | getColor | splash-color |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | tileColor | Color? | getColor | tile-color |
| [x] | selectedTileColor | Color? | getColor | selected-tile-color |
| [x] | enableFeedback | bool? | getBool | enable-feedback |
| [x] | horizontalTitleGap | double? | getDouble | horizontal-title-gap |
| [x] | minVerticalPadding | double? | getDouble | min-vertical-padding |
| [x] | minLeadingWidth | double? | getDouble | min-leading-width |
| [x] | minTileHeight | double? | getDouble | min-tile-height |
| [x] | titleAlignment | ListTileTitleAlignment? | getValue | title-alignment |
| [x] | internalAddSemanticForOnTap | bool | getBool | internal-add-semantic-for-on-tap |
| [x] | statesController | MaterialStatesController? | getValue | states-controller |
