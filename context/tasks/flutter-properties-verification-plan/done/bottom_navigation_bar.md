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
| [x] | items | List<BottomNavigationBarItem> | getValue | items |
| [x] | onTap | ValueChanged<int>? | getValue | on-tap |
| [x] | currentIndex | int | getInt | current-index |
| [x] | elevation | double | getDouble | elevation |
| [x] | type | BottomNavigationBarType? | getValue | type |
| [x] | fixedColor | Color? | getColor | fixed-color |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | iconSize | double | getDouble | icon-size |
| [x] | selectedItemColor | Color? | getColor | selected-item-color |
| [x] | unselectedItemColor | Color? | getColor | unselected-item-color |
| [x] | selectedIconTheme | IconThemeData? | getValue | selected-icon-theme |
| [x] | unselectedIconTheme | IconThemeData? | getValue | unselected-icon-theme |
| [x] | selectedLabelStyle | TextStyle? | getValue | selected-label-style |
| [x] | unselectedLabelStyle | TextStyle? | getValue | unselected-label-style |
| [x] | selectedFontSize | double | getDouble | selected-font-size |
| [x] | unselectedFontSize | double | getDouble | unselected-font-size |
| [x] | showSelectedLabels | bool? | getBool | show-selected-labels |
| [x] | showUnselectedLabels | bool? | getBool | show-unselected-labels |
| [x] | enableFeedback | bool? | getBool | enable-feedback |
| [x] | landscapeLayout | BottomNavigationBarLandscapeLayout? | getValue | landscape-layout |
