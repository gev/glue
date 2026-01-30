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

- [x] Slider widget

| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | value | double | getDouble | value |
| [x] | secondaryTrackValue | double? | getDouble | secondary-track-value |
| [x] | onChanged | ValueChanged<double>? | getValue | on-changed |
| [x] | onChangeStart | ValueChanged<double>? | getValue | on-change-start |
| [x] | onChangeEnd | ValueChanged<double>? | getValue | on-change-end |
| [x] | min | double | getDouble | min |
| [x] | max | double | getDouble | max |
| [x] | divisions | int? | getInt | divisions |
| [x] | label | String? | getString | label |
| [x] | activeColor | Color? | getColor | active-color |
| [x] | inactiveColor | Color? | getColor | inactive-color |
| [x] | secondaryActiveColor | Color? | getColor | secondary-active-color |
| [x] | thumbColor | Color? | getColor | thumb-color |
| [x] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | semanticFormatterCallback | SemanticFormatterCallback? | getValue | semantic-formatter-callback |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | allowedInteraction | SliderInteraction? | getValue | allowed-interaction |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | year2023 | bool? | getBool | year2023 |
