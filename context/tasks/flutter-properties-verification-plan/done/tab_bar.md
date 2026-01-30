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
| [x] | tabs | List<Widget> | getWidgets | tabs |
| [x] | controller | TabController? | getValue | controller |
| [x] | isScrollable | bool | getBool | is-scrollable |
| [x] | padding | EdgeInsetsGeometry? | getValue | padding |
| [x] | indicatorColor | Color? | getColor | indicator-color |
| [x] | automaticIndicatorColorAdjustment | bool | getBool | automatic-indicator-color-adjustment |
| [x] | indicatorWeight | double | getDouble | indicator-weight |
| [x] | indicatorPadding | EdgeInsetsGeometry | getValue | indicator-padding |
| [x] | indicator | Decoration? | getValue | indicator |
| [x] | indicatorSize | TabBarIndicatorSize? | getValue | indicator-size |
| [x] | dividerColor | Color? | getColor | divider-color |
| [x] | dividerHeight | double? | getDouble | divider-height |
| [x] | labelColor | Color? | getColor | label-color |
| [x] | labelStyle | TextStyle? | getValue | label-style |
| [x] | labelPadding | EdgeInsetsGeometry? | getValue | label-padding |
| [x] | unselectedLabelColor | Color? | getColor | unselected-label-color |
| [x] | unselectedLabelStyle | TextStyle? | getValue | unselected-label-style |
| [x] | dragStartBehavior | DragStartBehavior | getValue | drag-start-behavior |
| [x] | overlayColor | WidgetStateProperty<Color?>? | getValue | overlay-color |
| [x] | mouseCursor | MouseCursor? | getValue | mouse-cursor |
| [x] | enableFeedback | bool? | getBool | enable-feedback |
| [x] | onTap | ValueChanged<int>? | getValue | on-tap |
| [x] | onHover | TabValueChanged<bool>? | getValue | on-hover |
| [x] | onFocusChange | TabValueChanged<bool>? | getValue | on-focus-change |
| [x] | physics | ScrollPhysics? | getValue | physics |
| [x] | splashFactory | InteractiveInkFeatureFactory? | getValue | splash-factory |
| [x] | splashBorderRadius | BorderRadius? | getValue | splash-border-radius |
| [x] | tabAlignment | TabAlignment? | getValue | tab-alignment |
| [x] | textScaler | TextScaler? | getValue | text-scaler |
| [x] | indicatorAnimation | TabIndicatorAnimation? | getValue | indicator-animation |
