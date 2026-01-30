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
| [x] | controller | TextEditingController? | getValue | controller |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | hintText | String? | getString | hint-text |
| [x] | onTap | VoidCallback? | getVoidCallback | on-tap |
| [x] | onChanged | ValueChanged<String>? | getValue | on-changed |
| [x] | onSubmitted | ValueChanged<String>? | getValue | on-submitted |
| [x] | constraints | BoxConstraints? | getValue | constraints |
| [x] | elevation | WidgetStateProperty? | getValue | elevation |
| [x] | overlayColor | WidgetStateProperty? | getValue | overlay-color |
| [x] | side | BorderSide? | getValue | side |
| [x] | shape | OutlinedBorder? | getValue | shape |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | textStyle | TextStyle? | getValue | text-style |
| [x] | hintStyle | TextStyle? | getValue | hint-style |
| [x] | textCapitalization | TextCapitalization | getValue | text-capitalization |
| [x] | keyboardType | TextInputType | getValue | keyboard-type |

### Widget Status
- [x] SearchBar
