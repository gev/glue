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
| [x] | child | Widget? | properties.child | - |
| [x] | tooltip | String? | getString | tooltip |
| [x] | foregroundColor | Color? | getColor | foreground-color |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | focusColor | Color? | getColor | focus-color |
| [x] | hoverColor | Color? | getColor | hover-color |
| [x] | splashColor | Color? | getColor | splash-color |
| [x] | heroTag | Object? | getValue | hero-tag |
| [x] | elevation | double? | getDouble | elevation |
| [x] | focusElevation | double? | getDouble | focus-elevation |
| [x] | hoverElevation | double? | getDouble | hover-elevation |
| [x] | highlightElevation | double? | getDouble | highlight-elevation |
| [x] | disabledElevation | double? | getDouble | disabled-elevation |
| [x] | mini | bool? | getBool | mini |
| [x] | shape | OutlinedBorder? | getValue | shape |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | isExtended | bool | getBool | is-extended |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [x] | enableFeedback | bool? | getBool | enable-feedback |
| [x] | alignment | Alignment? | getValue | alignment |
| [x] | offset | Offset? | getValue | offset |
| [x] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [x] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | restorationId | String? | getString | restoration-id |
