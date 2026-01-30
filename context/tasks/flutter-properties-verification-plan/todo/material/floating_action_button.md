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
| [ ] | child | Widget? | properties.child | - |
| [ ] | tooltip | String? | getString | tooltip |
| [ ] | foregroundColor | Color? | getColor | foreground-color |
| [ ] | backgroundColor | Color? | getColor | background-color |
| [ ] | focusColor | Color? | getColor | focus-color |
| [ ] | hoverColor | Color? | getColor | hover-color |
| [ ] | splashColor | Color? | getColor | splash-color |
| [ ] | heroTag | Object? | getValue | hero-tag |
| [ ] | elevation | double? | getDouble | elevation |
| [ ] | focusElevation | double? | getDouble | focus-elevation |
| [ ] | hoverElevation | double? | getDouble | hover-elevation |
| [ ] | highlightElevation | double? | getDouble | highlight-elevation |
| [ ] | disabledElevation | double? | getDouble | disabled-elevation |
| [ ] | mini | bool? | getBool | mini |
| [ ] | shape | OutlinedBorder? | getValue | shape |
| [ ] | clipBehavior | Clip | getValue | clip-behavior |
| [ ] | isExtended | bool | getBool | is-extended |
| [ ] | autofocus | bool | getBool | autofocus |
| [ ] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [ ] | enableFeedback | bool? | getBool | enable-feedback |
| [ ] | alignment | Alignment? | getValue | alignment |
| [ ] | offset | Offset? | getValue | offset |
| [ ] | onPressed | VoidCallback? | getVoidCallback | on-pressed |
| [ ] | onLongPress | VoidCallback? | getVoidCallback | on-long-press |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | focusNode | FocusNode? | getValue | focus-node |
| [ ] | restorationId | String? | getString | restoration-id |
