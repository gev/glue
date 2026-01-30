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
| [x] | avatar | Widget? | getWidget | avatar |
| [x] | label | Widget | properties.child | - |
| [x] | labelStyle | TextStyle? | getValue | label-style |
| [x] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [x] | deleteIcon | Widget? | getWidget | delete-icon |
| [x] | onDeleted | VoidCallback? | getVoidCallback | on-deleted |
| [x] | deleteIconColor | Color? | getColor | delete-icon-color |
| [x] | deleteButtonTooltipMessage | String? | getString | delete-button-tooltip-message |
| [x] | side | BorderSide? | getValue | side |
| [x] | shape | OutlinedBorder? | getValue | shape |
| [x] | clipBehavior | Clip | getValue | clip-behavior |
| [x] | focusNode | FocusNode? | getValue | focus-node |
| [x] | autofocus | bool | getBool | autofocus |
| [x] | color | WidgetStateProperty<Color?>? | getValue | color |
| [x] | backgroundColor | Color? | getColor | background-color |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | visualDensity | VisualDensity? | getValue | visual-density |
| [x] | materialTapTargetSize | MaterialTapTargetSize? | getValue | material-tap-target-size |
| [x] | elevation | double? | getDouble | elevation |
| [x] | shadowColor | Color? | getColor | shadow-color |
| [x] | surfaceTintColor | Color? | getColor | surface-tint-color |
| [x] | iconTheme | IconThemeData? | getValue | icon-theme |
| [x] | avatarBoxConstraints | BoxConstraints? | getValue | avatar-box-constraints |
| [x] | deleteIconBoxConstraints | BoxConstraints? | getValue | delete-icon-box-constraints |
| [x] | chipAnimationStyle | ChipAnimationStyle? | getValue | chip-animation-style |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
