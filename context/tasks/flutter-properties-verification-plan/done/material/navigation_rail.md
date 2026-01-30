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
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | extended | bool | getBool | extended |
| [x] | leading | Widget? | getWidget | leading |
| [x] | trailing | Widget? | getWidget | trailing |
| [x] | destinations | List<NavigationRailDestination> | getValue | destinations |
| [x] | selectedIndex | int? | getInt | selected-index |
| [x] | onDestinationSelected | ValueChanged<int>? | getValue | on-destination-selected |
| [x] | elevation | double? | getDouble | elevation |
| [x] | groupAlignment | double? | getDouble | group-alignment |
| [x] | labelType | NavigationRailLabelType? | getValue | label-type |
| [x] | unselectedLabelTextStyle | TextStyle? | getValue | unselected-label-text-style |
| [x] | selectedLabelTextStyle | TextStyle? | getValue | selected-label-text-style |
| [x] | unselectedIconTheme | IconThemeData? | getValue | unselected-icon-theme |
| [x] | selectedIconTheme | IconThemeData? | getValue | selected-icon-theme |
| [x] | minWidth | double? | getDouble | min-width |
| [x] | minExtendedWidth | double? | getDouble | min-extended-width |
| [x] | useIndicator | bool? | getBool | use-indicator |
| [x] | indicatorColor | Color? | getColor | indicator-color |
| [x] | indicatorShape | ShapeBorder? | getValue | indicator-shape |
| [x] | leadingAtTop | bool | getBool | leading-at-top |
| [x] | trailingAtBottom | bool | getBool | trailing-at-bottom |
| [x] | scrollable | bool | getBool | scrollable |
