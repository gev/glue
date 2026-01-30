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
| [x] | items | List<DropdownMenuItem>? | getValue | items |
| [x] | selectedItemBuilder | DropdownButtonBuilder? | getValue | selected-item-builder |
| [x] | value | T? | getValue | value |
| [x] | hint | Widget? | getWidget | hint |
| [x] | disabledHint | Widget? | getWidget | disabled-hint |
| [x] | onChanged | ValueChanged? | getValue | on-changed |
| [x] | onTap | VoidCallback? | getVoidCallback | on-tap |
| [x] | elevation | int | getInt | elevation |
| [x] | style | TextStyle? | getValue | style |
| [x] | underline | Widget? | getWidget | underline |
| [x] | icon | Widget? | getWidget | icon |
| [x] | iconDisabledColor | Color? | getColor | icon-disabled-color |
| [x] | iconEnabledColor | Color? | getColor | icon-enabled-color |
| [x] | iconSize | double | getDouble | icon-size |
| [x] | isDense | bool | getBool | is-dense |
| [x] | isExpanded | bool | getBool | is-expanded |
| [x] | itemHeight | double? | getDouble | item-height |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | dropdownColor | Color? | getColor | color |
| [x] | menuMaxHeight | double? | getDouble | menu-max-height |
| [x] | enableFeedback | bool | getBool | enable-feedback |
| [x] | alignment | AlignmentGeometry | getValue | alignment |
| [x] | borderRadius | BorderRadius? | getValue | border-radius |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
