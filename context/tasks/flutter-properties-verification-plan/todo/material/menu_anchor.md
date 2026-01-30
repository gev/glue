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
| [ ] | controller | MenuController? | getValue | controller |
| [ ] | childFocusNode | FocusNode? | getValue | child-focus-node |
| [ ] | style | MenuStyle? | getValue | style |
| [ ] | alignmentOffset | Offset | getValue | alignment-offset |
| [ ] | reservedPadding | EdgeInsetsGeometry? | getValue | reserved-padding |
| [ ] | layerLink | LayerLink? | getValue | layer-link |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | anchorTapClosesMenu | bool | getBool | anchor-tap-closes-menu |
| [ ] | consumeOutsideTap | bool | getBool | consume-outside-tap |
| [ ] | onOpen | VoidCallback? | getVoidCallback | on-open |
| [ ] | onClose | VoidCallback? | getVoidCallback | on-close |
| [ ] | crossAxisUnconstrained | bool | getBool | cross-axis-unconstrained |
| [ ] | useRootOverlay | bool | getBool | use-root-overlay |
| [ ] | menuChildren | List<Widget> | getWidgets | menu-children |
| [ ] | builder | MenuAnchorChildBuilder? | getValue | builder |
| [ ] | child | Widget? | properties.child | - |
