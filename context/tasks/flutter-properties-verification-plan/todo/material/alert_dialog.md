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
| [ ] | icon | Widget? | getWidget | icon |
| [ ] | iconPadding | EdgeInsetsGeometry? | getValue | icon-padding |
| [ ] | iconColor | Color? | getColor | icon-color |
| [ ] | title | Widget? | getWidget | title |
| [ ] | titlePadding | EdgeInsetsGeometry? | getValue | title-padding |
| [ ] | titleTextStyle | TextStyle? | getValue | title-text-style |
| [ ] | content | Widget? | getWidget | content |
| [ ] | contentPadding | EdgeInsetsGeometry? | getValue | content-padding |
| [ ] | contentTextStyle | TextStyle? | getValue | content-text-style |
| [ ] | actions | List<Widget>? | getWidgets | actions |
| [ ] | actionsPadding | EdgeInsetsGeometry? | getValue | actions-padding |
| [ ] | actionsAlignment | MainAxisAlignment? | getValue | actions-alignment |
| [ ] | actionsOverflowAlignment | OverflowBarAlignment? | getValue | actions-overflow-alignment |
| [ ] | actionsOverflowDirection | VerticalDirection? | getValue | actions-overflow-direction |
| [ ] | actionsOverflowButtonSpacing | double? | getDouble | actions-overflow-button-spacing |
| [ ] | buttonPadding | EdgeInsetsGeometry? | getValue | button-padding |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | shadowColor | Color? | getColor | shadow-color |
| [ ] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [ ] | semanticLabel | String? | getString | semantic-label |
| [ ] | insetPadding | EdgeInsets? | getValue | inset-padding |
| [ ] | clipBehavior | Clip? | getValue | clip-behavior |
| [ ] | shape | ShapeBorder? | getValue | shape |
| [ ] | alignment | AlignmentGeometry? | getValue | alignment |
| [ ] | constraints | BoxConstraints? | getValue | constraints |
| [ ] | scrollable | bool | getBool | scrollable |
