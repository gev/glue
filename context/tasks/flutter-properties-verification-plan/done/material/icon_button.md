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
| [x] | icon | Widget? | properties.child | - |
| [x] | color | Color? | getColor | color |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | hoverColor | Color? | getColor | hover-color |
| [x] | highlightColor | Color? | getColor | highlight-color |
| [x] | splashColor | Color? | getColor | splash-color |
| [x] | disabledColor | Color? | getColor | disabled-color |
| [x] | iconSize | double | getDouble | icon-size |
| [x] | visualDensity | VisualDensity? | getValue | visual-density |
| [x] | padding | EdgeInsetsGeometry | getValue | padding |
| [x] | alignment | AlignmentGeometry | getValue | alignment |
| [x] | splashRadius | double? | getDouble | splash-radius |
| [x] | tooltip | String? | getString | tooltip |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | mini | bool? | getBool | mini |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [x] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | restorationId | String? | getString | restoration-id |

### Widget Status
[x] Widget implementation completed and verified
