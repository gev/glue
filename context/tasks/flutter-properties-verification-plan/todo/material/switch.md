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
| [ ] | value | bool | getBool | value |
| [ ] | onChanged | ValueChanged<bool>? | getValue | on-changed |
| [ ] | activeColor | Color? | getColor | active-color |
| [ ] | activeThumbColor | Color? | getColor | active-thumb-color |
| [ ] | activeTrackColor | Color? | getColor | active-track-color |
| [ ] | inactiveThumbColor | Color? | getColor | inactive-thumb-color |
| [ ] | inactiveTrackColor | Color? | getColor | inactive-track-color |
| [ ] | activeThumbImage | ImageProvider<Object>? | getValue | active-thumb-image |
| [ ] | onActiveThumbImageError | ImageErrorListener? | getValue | on-active-thumb-image-error |
| [ ] | inactiveThumbImage | ImageProvider<Object>? | getValue | inactive-thumb-image |
| [ ] | onInactiveThumbImageError | ImageErrorListener? | getValue | on-inactive-thumb-image-error |
| [ ] | thumbColor | WidgetStateProperty<Color?>? | getValue | thumb-color |
| [ ] | trackColor | WidgetStateProperty<Color?>? | getValue | track-color |
| [ ] | trackOutlineColor | WidgetStateProperty<Color?>? | getValue | track-outline-color |
| [ ] | trackOutlineWidth | WidgetStateProperty<double?>? | getValue | track-outline-width |
| [ ] | thumbIcon | WidgetStateProperty<Icon?>? | getValue | thumb-icon |
| [ ] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | hoverColor | Color? | getColor | hover-color |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | splashRadius | double? | getDouble | splash-radius |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
