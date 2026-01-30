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
| [ ] | scrollDirection | Axis | getValue | scroll-direction |
| [ ] | reverse | bool | getBool | reverse |
| [ ] | controller | ScrollController? | getValue | controller |
| [ ] | primary | bool? | getBool | primary |
| [ ] | physics | ScrollPhysics? | getValue | physics |
| [ ] | shrinkWrap | bool | getBool | shrink-wrap |
| [ ] | center | Key? | getKey | center |
| [ ] | anchor | double | getDouble | anchor |
| [ ] | cacheExtent | double? | getDouble | cache-extent |
| [ ] | slivers | List<Widget> | getWidgets | slivers |
| [ ] | semanticChildCount | int? | getInt | semantic-child-count |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | keyboardDismissBehavior | ScrollViewKeyboardDismissBehavior | getValue | keyboard-dismiss-behavior |
| [ ] | restorationId | String? | getString | restoration-id |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
