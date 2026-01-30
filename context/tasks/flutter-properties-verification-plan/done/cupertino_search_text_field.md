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
| [x] | onChanged | ValueChanged<String>? | getValue | on-changed |
| [x] | onSubmitted | ValueChanged<String>? | getValue | on-submitted |
| [x] | style | TextStyle? | getValue | style |
| [x] | placeholder | String? | getString | placeholder |
| [x] | placeholderStyle | TextStyle? | getValue | placeholder-style |
| [x] | decoration | BoxDecoration? | getValue | decoration |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | borderRadius | BorderRadius? | getValue | border-radius |
| [x] | padding | EdgeInsetsGeometry | getValue | padding |
| [x] | itemColor | Color? | getColor | item-color |
| [x] | itemSize | double? | getDouble | item-size |
| [x] | prefixIcon | Widget? | getWidget | prefix-icon |
| [x] | prefixMode | OverlayVisibilityMode | getValue | prefix-mode |
| [x] | suffixIcon | Widget? | getWidget | suffix-icon |
| [x] | suffixMode | OverlayVisibilityMode | getValue | suffix-mode |
| [x] | onSuffixTap | VoidCallback? | getVoidCallback | on-suffix-tap |
| [x] | enabled | bool? | getBool | enabled |
| [x] | autocorrect | bool | getBool | autocorrect |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
