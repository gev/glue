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
| [x] | animationController | AnimationController? | getValue | animation-controller |
| [x] | enableDrag | bool | getBool | enable-drag |
| [x] | showDragHandle | bool? | getBool | show-drag-handle |
| [x] | dragHandleColor | Color? | getColor | drag-handle-color |
| [x] | dragHandleSize | Size? | getValue | drag-handle-size |
| [x] | onDragStart | BottomSheetDragStartHandler? | getValue | on-drag-start |
| [x] | onDragEnd | BottomSheetDragEndHandler? | getValue | on-drag-end |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | elevation | double? | getDouble | elevation |
| [x] | shape | ShapeBorder? | getValue | shape |
| [x] | clipBehavior | Clip? | getValue | clip-behavior |
| [x] | constraints | BoxConstraints? | getValue | constraints |
| [x] | onClosing | VoidCallback | getVoidCallback | on-closing |
| [x] | builder | WidgetBuilder | getValue | builder |
