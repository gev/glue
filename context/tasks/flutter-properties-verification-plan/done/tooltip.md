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
| [x] | message | String? | getString | tooltip-message |
| [x] | richMessage | InlineSpan? | getValue | tooltip-rich-message |
| [x] | padding | EdgeInsetsGeometry? | getValue | tooltip-padding |
| [x] | margin | EdgeInsetsGeometry? | getValue | tooltip-margin |
| [x] | verticalOffset | double? | getDouble | tooltip-vertical-offset |
| [x] | preferBelow | bool? | getBool | tooltip-prefer-below |
| [x] | excludeFromSemantics | bool? | getBool | tooltip-exclude-from-semantics |
| [x] | decoration | Decoration? | getValue | tooltip-decoration |
| [x] | textStyle | TextStyle? | getValue | tooltip-text-style |
| [x] | textAlign | TextAlign? | getValue | tooltip-text-align |
| [x] | waitDuration | Duration? | getValue | tooltip-wait-duration |
| [x] | showDuration | Duration? | getValue | tooltip-show-duration |
| [x] | triggerMode | TooltipTriggerMode? | getValue | tooltip-trigger-mode |
| [x] | enableFeedback | bool? | getBool | tooltip-enable-feedback |
| [x] | onTriggered | VoidCallback? | getVoidCallback | tooltip-on-triggered |
| [x] | child | Widget? | properties.child | - |
