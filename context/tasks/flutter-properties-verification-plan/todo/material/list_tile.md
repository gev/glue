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
| [ ] | key | Key? | properties.key | - |
| [ ] | leading | Widget? | getWidget | leading |
| [ ] | title | Widget? | getWidget | title |
| [ ] | subtitle | Widget? | getWidget | subtitle |
| [ ] | trailing | Widget? | getWidget | trailing |
| [ ] | isThreeLine | bool? | getBool | is-three-line |
| [ ] | dense | bool? | getBool | dense |
| [ ] | visualDensity | VisualDensity? | getValue | visual-density |
| [ ] | shape | ShapeBorder? | getValue | shape |
| [ ] | style | ListTileStyle? | getValue | style |
| [ ] | selectedColor | Color? | getColor | selected-color |
| [ ] | iconColor | Color? | getColor | icon-color |
| [ ] | textColor | Color? | getColor | text-color |
| [ ] | titleTextStyle | TextStyle? | getValue | title-text-style |
| [ ] | subtitleTextStyle | TextStyle? | getValue | subtitle-text-style |
| [ ] | leadingAndTrailingTextStyle | TextStyle? | getValue | leading-and-trailing-text-style |
| [ ] | contentPadding | EdgeInsetsGeometry? | getValue | content-padding |
| [ ] | enabled | bool | getBool | enabled |
| [ ] | onTap | GestureTapCallback? | getVoidCallback | on-tap |
| [ ] | onLongPress | GestureLongPressCallback? | getValue | on-long-press |
| [ ] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | selected | bool | getBool | selected |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | hoverColor | Color? | getColor | hover-color |
| [ ] | splashColor | Color? | getColor | splash-color |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | tileColor | Color? | getColor | tile-color |
| [ ] | selectedTileColor | Color? | getColor | selected-tile-color |
| [ ] | enableFeedback | bool? | getBool | enable-feedback |
| [ ] | horizontalTitleGap | double? | getDouble | horizontal-title-gap |
| [ ] | minVerticalPadding | double? | getDouble | min-vertical-padding |
| [ ] | minLeadingWidth | double? | getDouble | min-leading-width |
| [ ] | minTileHeight | double? | getDouble | min-tile-height |
| [ ] | titleAlignment | ListTileTitleAlignment? | getValue | title-alignment |
| [ ] | internalAddSemanticForOnTap | bool | getBool | internal-add-semantic-for-on-tap |
| [ ] | statesController | MaterialStatesController? | getValue | states-controller |
