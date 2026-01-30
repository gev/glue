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
| [x] | avatar | Widget? | getValue | avatar |
| [x] | label | Widget | getValue | label |
| [x] | labelStyle | TextStyle? | getValue | label-style |
| [x] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [x] | pressElevation | double? | getValue | press-elevation |
| [x] | side | BorderSide? | getValue | side |
| [x] | shape | OutlinedBorder? | getValue | shape |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | disabledColor | Color? | getColor | disabled-color |
| [x] | avatarBoxConstraints | BoxConstraints? | getValue | avatar-box-constraints |
