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
| [ ] | value | double | getDouble | value |
| [ ] | secondaryTrackValue | double? | getDouble | secondary-track-value |
| [ ] | onChanged | ValueChanged<double>? | getValue | on-changed |
| [ ] | onChangeStart | ValueChanged<double>? | getValue | on-change-start |
| [ ] | onChangeEnd | ValueChanged<double>? | getValue | on-change-end |
| [ ] | min | double | getDouble | min |
| [ ] | max | double | getDouble | max |
| [ ] | divisions | int? | getInt | divisions |
| [ ] | label | String? | getString | label |
| [ ] | activeColor | Color? | getColor | active-color |
| [ ] | inactiveColor | Color? | getColor | inactive-color |
| [ ] | secondaryActiveColor | Color? | getColor | secondary-active-color |
| [ ] | thumbColor | Color? | getColor | thumb-color |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | semanticFormatterCallback | SemanticFormatterCallback? | getValue | semantic-formatter-callback |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | allowedInteraction | SliderInteraction? | getValue | allowed-interaction |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | year2023 | bool? | getBool | year2023 |
