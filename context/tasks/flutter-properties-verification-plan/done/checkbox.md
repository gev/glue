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
| [x] | value | bool? | getBool | value |
| [x] | tristate | bool | getBool | tristate |
| [x] | onChanged | ValueChanged<bool?>? | getValue | on-changed |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | activeColor | Color? | getColor | active-color |
| [x] | fillColor | WidgetStateProperty<Color?>? | getValue | fill-color |
| [x] | checkColor | Color? | getColor | check-color |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | hoverColor | Color? | getColor | hover-color |
| [x] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [x] | splashRadius | double? | getDouble | splash-radius |
| [x] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [x] | visualDensity | VisualDensity? | getValue | visual-density |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | shape | OutlinedBorder? | getValue | shape |
| [x] | side | BorderSide? | getValue | side |
| [x] | isError | bool | getBool | is-error |
| [x] | semanticLabel | String? | getString | semantic-label |
