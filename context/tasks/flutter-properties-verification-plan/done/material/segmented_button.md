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

### SegmentedButton Verification Complete [x]

| todo check | flutter property name | flutter type | property getter/method without parameters | property name |
|------------|----------------------|--------------|------------------------------------------|---------------|
| [x] | key | Key? | properties.key | - |
| [x] | selected | Set<T> | getValue | selected |
| [x] | segments | List<Widget> | getWidgets | segments |
| [x] | onSelectionChanged | ValueChanged<Set<T>> | getValue | on-selection-changed |
| [x] | multiSelectionEnabledFor | Set<T>? | getValue | multi-selection-enabled-for |
| [x] | showSelectedIcon | bool? | getBool | show-selected-icon |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | unselectedColor | Color? | getColor | unselected-color |
| [x] | selectedColor | Color? | getColor | selected-color |
| [x] | disabledColor | Color? | getColor | disabled-color |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | visualDensity | VisualDensity? | getValue | visual-density |
| [x] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
