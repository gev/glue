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
| [ ] | icon | Widget? | properties.child | - |
| [ ] | color | Color? | getColor | color |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | hoverColor | Color? | getColor | hover-color |
| [ ] | highlightColor | Color? | getColor | highlight-color |
| [ ] | splashColor | Color? | getColor | splash-color |
| [ ] | disabledColor | Color? | getColor | disabled-color |
| [ ] | iconSize | double | getDouble | icon-size |
| [ ] | visualDensity | VisualDensity? | getValue | visual-density |
| [ ] | padding | EdgeInsetsGeometry | getValue | padding |
| [ ] | alignment | AlignmentGeometry | getValue | alignment |
| [ ] | splashRadius | double? | getDouble | splash-radius |
| [ ] | tooltip | String? | getString | tooltip |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | mini | bool? | getBool | mini |
| [ ] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [ ] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | restorationId | String? | getString | restoration-id |
