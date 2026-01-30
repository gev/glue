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
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | extended | bool | getBool | extended |
| [ ] | leading | Widget? | getWidget | leading |
| [ ] | trailing | Widget? | getWidget | trailing |
| [ ] | destinations | List<NavigationRailDestination> | getValue | destinations |
| [ ] | selectedIndex | int? | getInt | selected-index |
| [ ] | onDestinationSelected | ValueChanged<int>? | getValue | on-destination-selected |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | groupAlignment | double? | getDouble | group-alignment |
| [ ] | labelType | NavigationRailLabelType? | getValue | label-type |
| [ ] | unselectedLabelTextStyle | TextStyle? | getValue | unselected-label-text-style |
| [ ] | selectedLabelTextStyle | TextStyle? | getValue | selected-label-text-style |
| [ ] | unselectedIconTheme | IconThemeData? | getValue | unselected-icon-theme |
| [ ] | selectedIconTheme | IconThemeData? | getValue | selected-icon-theme |
| [ ] | minWidth | double? | getDouble | min-width |
| [ ] | minExtendedWidth | double? | getDouble | min-extended-width |
| [ ] | useIndicator | bool? | getBool | use-indicator |
| [ ] | indicatorColor | Color? | getColor | indicator-color |
| [ ] | indicatorShape | ShapeBorder? | getValue | indicator-shape |
| [ ] | leadingAtTop | bool | getBool | leading-at-top |
| [ ] | trailingAtBottom | bool | getBool | trailing-at-bottom |
| [ ] | scrollable | bool | getBool | scrollable |
