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

- [x] Switch widget

| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | value | bool | getBool | value |
| [x] | onChanged | ValueChanged<bool>? | getValue | on-changed |
| [x] | activeColor | Color? | getColor | active-color |
| [x] | activeThumbColor | Color? | getColor | active-thumb-color |
| [x] | activeTrackColor | Color? | getColor | active-track-color |
| [x] | inactiveThumbColor | Color? | getColor | inactive-thumb-color |
| [x] | inactiveTrackColor | Color? | getColor | inactive-track-color |
| [x] | activeThumbImage | ImageProvider<Object>? | getValue | active-thumb-image |
| [x] | onActiveThumbImageError | ImageErrorListener? | getValue | on-active-thumb-image-error |
| [x] | inactiveThumbImage | ImageProvider<Object>? | getValue | inactive-thumb-image |
| [x] | onInactiveThumbImageError | ImageErrorListener? | getValue | on-inactive-thumb-image-error |
| [x] | thumbColor | WidgetStateProperty<Color?>? | getValue | thumb-color |
| [x] | trackColor | WidgetStateProperty<Color?>? | getValue | track-color |
| [x] | trackOutlineColor | WidgetStateProperty<Color?>? | getValue | track-outline-color |
| [x] | trackOutlineWidth | WidgetStateProperty<double?>? | getValue | track-outline-width |
| [x] | thumbIcon | WidgetStateProperty<Icon?>? | getValue | thumb-icon |
| [x] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | hoverColor | Color? | getColor | hover-color |
| [x] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [x] | splashRadius | double? | getDouble | splash-radius |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | onFocusChange | ValueChanged<bool>? | getValue | on-focus-change |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
