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
| [ ] | controller | TextEditingController? | getValue | controller |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | hintText | String? | getString | hint-text |
| [ ] | onTap | VoidCallback? | getVoidCallback | on-tap |
| [ ] | onChanged | ValueChanged<String>? | getValue | on-changed |
| [ ] | onSubmitted | ValueChanged<String>? | getValue | on-submitted |
| [ ] | constraints | BoxConstraints? | getValue | constraints |
| [ ] | elevation | WidgetStateProperty? | getValue | elevation |
| [ ] | overlayColor | WidgetStateProperty? | getValue | overlay-color |
| [ ] | side | BorderSide? | getValue | side |
| [ ] | shape | OutlinedBorder? | getValue | shape |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | textStyle | TextStyle? | getValue | text-style |
| [ ] | hintStyle | TextStyle? | getValue | hint-style |
| [ ] | textCapitalization | TextCapitalization | getValue | text-capitalization |
| [ ] | keyboardType | TextInputType | getValue | keyboard-type |
