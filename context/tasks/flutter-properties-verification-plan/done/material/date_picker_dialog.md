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
| [x] | initialDate | DateTime? | getValue | initial-date |
| [x] | firstDate | DateTime | getValue | first-date |
| [x] | lastDate | DateTime | getValue | last-date |
| [x] | currentDate | DateTime? | getValue | current-date |
| [x] | initialEntryMode | DatePickerEntryMode | getValue | initial-entry-mode |
| [x] | selectableDayPredicate | SelectableDayPredicate? | getValue | selectable-day-predicate |
| [x] | cancelText | String? | getString | cancel-text |
| [x] | confirmText | String? | getString | confirm-text |
| [x] | helpText | String? | getString | help-text |
| [x] | errorFormatText | String? | getString | error-format-text |
| [x] | errorInvalidText | String? | getString | error-invalid-text |
| [x] | fieldHintText | String? | getString | field-hint-text |
| [x] | fieldLabelText | String? | getString | field-label-text |
| [x] | keyboardType | TextInputType? | getValue | keyboard-type |
| [x] | restorationId | String? | getString | restoration-id |
| [x] | onDatePickerModeChange | ValueChanged<DatePickerEntryMode>? | getValue | on-date-picker-mode-change |
| [x] | switchToInputEntryModeIcon | Icon? | getValue | switch-to-input-entry-mode-icon |
| [x] | switchToCalendarEntryModeIcon | Icon? | getValue | switch-to-calendar-entry-mode-icon |
| [x] | insetPadding | EdgeInsets | getValue | inset-padding |
| [x] | calendarDelegate | CalendarDelegate<DateTime> | getValue | calendar-delegate |

[x] DatePickerDialog
