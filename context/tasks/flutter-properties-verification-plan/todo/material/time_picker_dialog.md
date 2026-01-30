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
| [ ] | initialTime | TimeOfDay | getValue | initial-time |
| [ ] | cancelText | String? | getString | cancel-text |
| [ ] | confirmText | String? | getString | confirm-text |
| [ ] | helpText | String? | getString | help-text |
| [ ] | errorInvalidText | String? | getString | error-invalid-text |
| [ ] | hourLabelText | String? | getString | hour-label-text |
| [ ] | minuteLabelText | String? | getString | minute-label-text |
| [ ] | restorationId | String? | getString | restoration-id |
| [ ] | initialEntryMode | TimePickerEntryMode | getValue | initial-entry-mode |
| [ ] | orientation | Orientation? | getValue | orientation |
| [ ] | onEntryModeChanged | EntryModeChangeCallback? | getValue | on-entry-mode-changed |
| [ ] | switchToInputEntryModeIcon | Icon? | getValue | switch-to-input-entry-mode-icon |
| [ ] | switchToTimerEntryModeIcon | Icon? | getValue | switch-to-timer-entry-mode-icon |
| [ ] | emptyInitialInput | bool | getBool | empty-initial-input |
