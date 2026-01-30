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
| [x] | scrollDirection | Axis | getValue | scroll-direction |
| [x] | reverse | bool | getBool | reverse |
| [x] | controller | ScrollController? | getValue | controller |
| [x] | primary | bool? | getBool | primary |
| [x] | physics | ScrollPhysics? | getValue | physics |
| [x] | shrinkWrap | bool | getBool | shrink-wrap |
| [x] | center | Key? | getKey | center |
| [x] | anchor | double | getDouble | anchor |
| [x] | cacheExtent | double? | getDouble | cache-extent |
| [x] | slivers | List<Widget> | getWidgets | slivers |
| [x] | semanticChildCount | int? | getInt | semantic-child-count |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | keyboardDismissBehavior | ScrollViewKeyboardDismissBehavior | getValue | keyboard-dismiss-behavior |
| [x] | restorationId | String? | getString | restoration-id |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
