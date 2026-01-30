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
| [ ] | tabs | List<Widget> | getWidgets | tabs |
| [ ] | controller | TabController? | getValue | controller |
| [ ] | isScrollable | bool | getBool | is-scrollable |
| [ ] | padding | EdgeInsetsGeometry? | getValue | padding |
| [ ] | indicatorColor | Color? | getColor | indicator-color |
| [ ] | automaticIndicatorColorAdjustment | bool | getBool | automatic-indicator-color-adjustment |
| [ ] | indicatorWeight | double | getDouble | indicator-weight |
| [ ] | indicatorPadding | EdgeInsetsGeometry | getValue | indicator-padding |
| [ ] | indicator | Decoration? | getValue | indicator |
| [ ] | indicatorSize | TabBarIndicatorSize? | getValue | indicator-size |
| [ ] | dividerColor | Color? | getColor | divider-color |
| [ ] | dividerHeight | double? | getDouble | divider-height |
| [ ] | labelColor | Color? | getColor | label-color |
| [ ] | labelStyle | TextStyle? | getValue | label-style |
| [ ] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [ ] | unselectedLabelColor | Color? | getColor | unselected-label-color |
| [ ] | unselectedLabelStyle | TextStyle? | getValue | unselected-label-style |
| [ ] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [ ] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [ ] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [ ] | enableFeedback | bool? | getBool | enable-feedback |
| [ ] | onTap | ValueChanged<int>? | getValue | on-tap |
| [ ] | onHover | TabValueChanged<bool>? | getValue | on-hover |
| [ ] | onFocusChange | TabValueChanged<bool>? | getValue | on-focus-change |
| [ ] | physics | ScrollPhysics? | getValue | physics |
| [ ] | splashFactory | InteractiveInkFeatureFactory? | getValue | splash-factory |
| [ ] | splashBorderRadius | BorderRadius? | getValue | splash-border-radius |
| [ ] | tabAlignment | TabAlignment? | getValue | tab-alignment |
| [ ] | textScaler | TextScaler? | getValue | text-scaler |
| [ ] | indicatorAnimation | TabIndicatorAnimation? | getValue | indicator-animation |
