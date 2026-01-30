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
| [ ] | items | List<BottomNavigationBarItem> | getValue | items |
| [ ] | onTap | ValueChanged<int>? | getValue | on-tap |
| [ ] | currentIndex | int | getInt | current-index |
| [ ] | elevation | double | getDouble | elevation |
| [ ] | type | BottomNavigationBarType? | getValue | type |
| [ ] | fixedColor | Color? | getColor | fixed-color |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | iconSize | double | getDouble | icon-size |
| [ ] | selectedItemColor | Color? | getColor | selected-item-color |
| [ ] | unselectedItemColor | Color? | getColor | unselected-item-color |
| [ ] | selectedIconTheme | IconThemeData? | getValue | selected-icon-theme |
| [ ] | unselectedIconTheme | IconThemeData? | getValue | unselected-icon-theme |
| [ ] | selectedLabelStyle | TextStyle? | getValue | selected-label-style |
| [ ] | unselectedLabelStyle | TextStyle? | getValue | unselected-label-style |
| [ ] | selectedFontSize | double | getDouble | selected-font-size |
| [ ] | unselectedFontSize | double | getDouble | unselected-font-size |
| [ ] | showSelectedLabels | bool? | getBool | show-selected-labels |
| [ ] | showUnselectedLabels | bool? | getBool | show-unselected-labels |
| [ ] | enableFeedback | bool? | getBool | enable-feedback |
| [ ] | landscapeLayout | BottomNavigationBarLandscapeLayout? | getValue | landscape-layout |
