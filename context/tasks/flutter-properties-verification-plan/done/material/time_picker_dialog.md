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
| [x] | initialTime | TimeOfDay | getValue | initial-time |
| [x] | cancelText | String? | getString | cancel-text |
| [x] | confirmText | String? | getString | confirm-text |
| [x] | helpText | String? | getString | help-text |
| [x] | errorInvalidText | String? | getString | error-invalid-text |
| [x] | hourLabelText | String? | getString | hour-label-text |
| [x] | minuteLabelText | String? | getString | minute-label-text |
| [x] | restorationId | String? | getString | restoration-id |
| [x] | initialEntryMode | TimePickerEntryMode | getValue | initial-entry-mode |
| [x] | orientation | Orientation? | getValue | orientation |
| [x] | onEntryModeChanged | EntryModeChangeCallback? | getValue | on-entry-mode-changed |
| [x] | switchToInputEntryModeIcon | Icon? | getValue | switch-to-input-entry-mode-icon |
| [x] | switchToTimerEntryModeIcon | Icon? | getValue | switch-to-timer-entry-mode-icon |
| [x] | emptyInitialInput | bool | getBool | empty-initial-input |

## Widget Status: done
