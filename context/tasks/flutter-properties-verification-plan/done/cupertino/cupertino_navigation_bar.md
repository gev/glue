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
| [x] | leading | Widget? | getWidget | leading |
| [x] | automaticallyImplyLeading | bool | getBool | automatically-imply-leading |
| [x] | automaticallyImplyMiddle | bool | getBool | automatically-imply-middle |
| [x] | previousPageTitle | String? | getString | previous-page-title |
| [x] | middle | Widget? | getWidget | middle |
| [x] | trailing | Widget? | getWidget | trailing |
| [x] | border | Border? | getValue | border |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | brightness | Brightness? | getValue | brightness |
| [x] | padding | EdgeInsetsDirectional? | getValue | padding |
| [x] | transitionBetweenRoutes | bool | getBool | transition-between-routes |
| [x] | heroTag | Object | getValue | hero-tag |

#### Widget Implementation Status
- [x] CupertinoNavigationBar widget completed
