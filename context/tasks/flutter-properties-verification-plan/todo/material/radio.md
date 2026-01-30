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
| [ ] | value | T | getValue | value |
| [ ] | groupValue | T? | getValue | group-value |
| [ ] | onChanged | ValueChanged<T?>? | getValue | on-changed |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | toggleable | bool | getBool | toggleable |
| [ ] | activeColor | Color? | getColor | active-color |
| [ ] | fillColor | WidgetStateProperty<Color?>? | getValue | fill-color |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | hoverColor | Color? | getColor | hover-color |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | splashRadius | double? | getDouble | splash-radius |
| [ ] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [ ] | visualDensity | VisualDensity? | getValue | visual-density |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | enabled | bool? | getBool | enabled |
| [ ] | groupRegistry | RadioGroupRegistry<T>? | getValue | group-registry |
| [ ] | backgroundColor | WidgetStateProperty<Color?>? | getValue | background-color |
| [ ] | side | BorderSide? | getValue | side |
| [ ] | innerRadius | WidgetStateProperty<double?>? | getValue | inner-radius |
