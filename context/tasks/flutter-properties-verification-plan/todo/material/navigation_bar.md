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
| [ ] | animationDuration | Duration? | getValue | animation-duration |
| [ ] | selectedIndex | int | getInt | selected-index |
| [ ] | destinations | List<Widget> | getWidgets | destinations |
| [ ] | onDestinationSelected | ValueChanged<int>? | getValue | on-destination-selected |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [ ] | indicatorColor | Color? | getColor | indicator-color |
| [ ] | indicatorShape | ShapeBorder? | getValue | indicator-shape |
| [ ] | height | double? | getDouble | height |
| [ ] | labelBehavior | NavigationDestinationLabelBehavior? | getValue | label-behavior |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | labelTextStyle | WidgetStateProperty<TextStyle?>? | getValue | label-text-style |
| [ ] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [ ] | maintainBottomViewPadding | bool | getBool | maintain-bottom-view-padding |
