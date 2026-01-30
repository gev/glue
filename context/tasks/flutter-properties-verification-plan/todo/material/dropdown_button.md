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
| [ ] | items | List<DropdownMenuItem>? | getValue | items |
| [ ] | selectedItemBuilder | DropdownButtonBuilder? | getValue | selected-item-builder |
| [ ] | value | T? | getValue | value |
| [ ] | hint | Widget? | getWidget | hint |
| [ ] | disabledHint | Widget? | getWidget | disabled-hint |
| [ ] | onChanged | ValueChanged? | getValue | on-changed |
| [ ] | onTap | VoidCallback? | getVoidCallback | on-tap |
| [ ] | elevation | int | getInt | elevation |
| [ ] | style | TextStyle? | getValue | style |
| [ ] | underline | Widget? | getWidget | underline |
| [ ] | icon | Widget? | getWidget | icon |
| [ ] | iconDisabledColor | Color? | getColor | icon-disabled-color |
| [ ] | iconEnabledColor | Color? | getColor | icon-enabled-color |
| [ ] | iconSize | double | getDouble | icon-size |
| [ ] | isDense | bool | getBool | is-dense |
| [ ] | isExpanded | bool | getBool | is-expanded |
| [ ] | itemHeight | double? | getDouble | item-height |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | dropdownColor | Color? | getColor | color |
| [ ] | menuMaxHeight | double? | getDouble | menu-max-height |
| [ ] | enableFeedback | bool | getBool | enable-feedback |
| [ ] | alignment | AlignmentGeometry | getValue | alignment |
| [ ] | borderRadius | BorderRadius? | getValue | border-radius |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
