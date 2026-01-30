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
| [x] | icon | Widget? | getWidget | icon |
| [x] | iconPadding | EdgeInsetsGeometry? | getValue | icon-padding |
| [x] | iconColor | Color? | getColor | icon-color |
| [x] | title | Widget? | getWidget | title |
| [x] | titlePadding | EdgeInsetsGeometry? | getValue | title-padding |
| [x] | titleTextStyle | TextStyle? | getValue | title-text-style |
| [x] | content | Widget? | getWidget | content |
| [x] | contentPadding | EdgeInsetsGeometry? | getValue | content-padding |
| [x] | contentTextStyle | TextStyle? | getValue | content-text-style |
| [x] | actions | List<Widget>? | getWidgets | actions |
| [x] | actionsPadding | EdgeInsetsGeometry? | getValue | actions-padding |
| [x] | actionsAlignment | MainAxisAlignment? | getValue | actions-alignment |
| [x] | actionsOverflowAlignment | OverflowBarAlignment? | getValue | actions-overflow-alignment |
| [x] | actionsOverflowDirection | VerticalDirection? | getValue | actions-overflow-direction |
| [x] | actionsOverflowButtonSpacing | double? | getDouble | actions-overflow-button-spacing |
| [x] | buttonPadding | EdgeInsetsGeometry? | getValue | button-padding |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | semanticLabel | String? | getString | semantic-label |
| [x] | insetPadding | EdgeInsets? | getValue | inset-padding |
| [x] | clipBehavior | Clip? | getValue | clip-behavior |
| [x] | shape | ShapeBorder? | getValue | shape |
| [x] | alignment | AlignmentGeometry? | getValue | alignment |
| [x] | constraints | BoxConstraints? | getValue | constraints |
| [x] | scrollable | bool | getBool | scrollable |

### Widget Status
[x] Widget implementation completed and verified
